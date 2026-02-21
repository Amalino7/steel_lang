mod display;
pub mod type_defs;

use crate::scanner::Span;
use crate::typechecker::core::error::TypeCheckerError;
use std::collections::HashMap;
use std::rc::Rc;

pub type Symbol = Rc<str>;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub is_vararg: bool,
    pub params: Vec<(Symbol, Type)>,
    pub return_type: Type,
    pub type_params: Vec<Symbol>,
}

impl FunctionType {
    pub fn is_static(&self) -> bool {
        self.params
            .first()
            .map(|param| param.0.as_ref() != "self")
            .unwrap_or(true)
    }
}

impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        self.return_type == other.return_type
            && self.params.len() == other.params.len()
            && self.is_vararg == other.is_vararg
            && self
                .params
                .iter()
                .zip(&other.params)
                .all(|(a, b)| a.1 == b.1)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleType {
    pub types: Vec<Type>,
}

// TODO use Rc<[Type]>
pub type GenericArgs = Rc<Vec<Type>>;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Nil,
    Number,
    String,
    Boolean,
    Void,
    Never,
    Error,
    Infer(u32),
    Optional(Box<Type>),
    Unknown,
    Function(Rc<FunctionType>),
    Tuple(Rc<TupleType>),
    GenericParam(Symbol),
    Metatype(Symbol, GenericArgs),
    Struct(Symbol, GenericArgs),
    Interface(Symbol),
    Enum(Symbol, GenericArgs),
    List(GenericArgs),
    Map(GenericArgs),
    Any,
}

impl Type {
    pub fn new_list(element: Type) -> Type {
        Type::List(Rc::new(vec![element]))
    }

    pub fn new_map(key: Type, value: Type) -> Type {
        Type::Map(Rc::new(vec![key, value]))
    }

    pub fn list_element(&self) -> Option<&Type> {
        if let Type::List(args) = self {
            args.get(0)
        } else {
            None
        }
    }

    pub fn map_key(&self) -> Option<&Type> {
        if let Type::Map(args) = self {
            args.get(0)
        } else {
            None
        }
    }

    pub fn map_value(&self) -> Option<&Type> {
        if let Type::Map(args) = self {
            args.get(1)
        } else {
            None
        }
    }

    pub fn generic_args(&self) -> &[Type] {
        match self {
            Type::Struct(_, args) | Type::Enum(_, args) | Type::List(args) | Type::Map(args) => {
                args.as_slice()
            }
            _ => &[],
        }
    }

    pub fn generic_count(&self) -> usize {
        self.generic_args().len()
    }
}

impl Type {
    pub fn transform(
        self,
        f: &impl Fn(Type) -> Type,
        u: &impl Fn(&[Symbol]) -> Vec<Symbol>,
    ) -> Type {
        let transformed = match self {
            Type::Nil
            | Type::Number
            | Type::String
            | Type::Boolean
            | Type::Void
            | Type::Never
            | Type::Error
            | Type::Infer(_)
            | Type::Unknown
            | Type::Any
            | Type::GenericParam(_)
            | Type::Metatype(_, _)
            | Type::Interface(_) => self,

            Type::Optional(inner) => Type::Optional(Box::new(inner.transform(f, u))),
            Type::List(args) => {
                let new_args = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::List(Rc::new(new_args))
            }
            Type::Map(args) => {
                let new_args = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Map(Rc::new(new_args))
            }
            Type::Tuple(tt) => {
                let types = tt.types.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Tuple(Rc::new(TupleType { types }))
            }
            Type::Function(ft) => {
                let params = ft
                    .params
                    .iter()
                    .map(|(name, ty)| (name.clone(), ty.clone().transform(f, u)))
                    .collect();
                let ret = ft.return_type.clone().transform(f, u);
                Type::new_general_function(params, ret, u(&ft.type_params), ft.is_vararg)
            }
            Type::Struct(name, args) => {
                let args = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Struct(name.clone(), Rc::new(args))
            }
            Type::Enum(name, args) => {
                let args = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Enum(name.clone(), Rc::new(args))
            }
        };
        f(transformed)
    }

    pub fn any(&self, f: &impl Fn(&Type) -> bool) -> bool {
        if f(self) {
            return true;
        }
        match self {
            Type::Optional(inner) => inner.any(f),
            Type::List(args) | Type::Map(args) => args.iter().any(|t| t.any(f)),
            Type::Tuple(tt) => tt.types.iter().any(|t| t.any(f)),
            Type::Function(ft) => ft.params.iter().any(|(_, t)| t.any(f)) || ft.return_type.any(f),
            Type::Struct(_, args) | Type::Enum(_, args) => args.iter().any(|t| t.any(f)),
            _ => false,
        }
    }

    pub fn all(&self, f: &impl Fn(&Type) -> bool) -> bool {
        if !f(self) {
            return false;
        }
        match self {
            Type::Optional(inner) => inner.all(f),
            Type::List(args) | Type::Map(args) => args.iter().all(|t| t.all(f)),
            Type::Tuple(tt) => tt.types.iter().all(|t| t.all(f)),
            Type::Function(ft) => ft.params.iter().all(|(_, t)| t.all(f)) && ft.return_type.all(f),
            Type::Struct(_, args) | Type::Enum(_, args) => args.iter().all(|t| t.all(f)),
            _ => true,
        }
    }
}

impl Type {
    pub fn is_concrete(&self) -> bool {
        self.all(&|ty| !matches!(ty, Type::Infer(_) | Type::Unknown | Type::Metatype(..)))
    }

    pub fn unwrap_optional_safe(&self, safe: bool, span: Span) -> Result<Type, TypeCheckerError> {
        if safe {
            match self {
                Type::Optional(inner) => Ok(inner.as_ref().clone()),
                _ => Err(TypeCheckerError::TypeMismatch {
                    expected: Type::Optional(Box::new(Type::Any)),
                    found: self.clone(),
                    span,
                    message: "Cannot access safe navigation of non-optional type. Remove ?.",
                }),
            }
        } else {
            Ok(self.clone())
        }
    }

    /// Unwraps optional for safe access, emitting a warning instead of error if used on non-optional
    pub fn unwrap_optional_safe_warn(
        &self,
        safe: bool,
        span: Span,
        warnings: &mut Vec<crate::typechecker::core::error::TypeCheckerWarning>,
    ) -> Type {
        if safe {
            match self {
                Type::Optional(inner) => inner.as_ref().clone(),
                _ => {
                    warnings.push(crate::typechecker::core::error::TypeCheckerWarning::SafeAccessOnNonOptional { span });
                    self.clone()
                }
            }
        } else {
            self.clone()
        }
    }

    pub fn wrap_in_optional(self) -> Type {
        match self {
            Type::Optional(_) => self,
            _ => Type::Optional(Box::new(self)),
        }
    }

    pub fn get_name(&self) -> Option<&str> {
        match self {
            Type::Error => None,
            Type::Infer(_) => None,
            Type::Interface(name) => Some(name),
            Type::Number => Some("number"),
            Type::String => Some("string"),
            Type::Boolean => Some("boolean"),
            Type::Void => Some("void"),
            Type::Never => Some("never"),
            Type::Function(_) => None,
            Type::Unknown => None,
            Type::Struct(name, _) => Some(name),
            Type::Any => None,
            Type::Optional(_) => None,
            Type::Nil => Some("nil"),
            Type::GenericParam(_) => None,
            Type::Enum(name, _) => Some(name),
            Type::Tuple(_) => None,
            Type::Metatype(_, _) => None,
            Type::List(_) => Some("List"),
            Type::Map(_) => Some("Map"),
        }
    }

    pub fn new_general_function(
        params: Vec<(Symbol, Type)>,
        return_type: Type,
        type_params: Vec<Symbol>,
        is_vararg: bool,
    ) -> Self {
        Type::Function(Rc::new(FunctionType {
            is_vararg,
            params,
            return_type,
            type_params,
        }))
    }

    pub fn new_function(
        params: Vec<(Symbol, Type)>,
        return_type: Type,
        type_params: Vec<Symbol>,
    ) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_vararg: false,
            params,
            return_type,
            type_params,
        }))
    }

    pub fn new_tuple(elements: Vec<Type>) -> Type {
        Type::Tuple(Rc::new(TupleType { types: elements }))
    }

    pub fn new_vararg(param_types: Vec<Type>, return_type: Type) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_vararg: true,
            params: param_types.into_iter().map(|e| ("_".into(), e)).collect(),
            return_type,
            type_params: vec![],
        }))
    }

    // TODO change can compare logic
    pub fn can_compare(left: &Type, right: &Type) -> bool {
        if let Type::Optional(inner) = left {
            if *right == Type::Nil {
                true
            } else {
                Self::can_compare(inner, right)
            }
        } else if let Type::Optional(inner) = right {
            if *left == Type::Nil {
                true
            } else {
                Self::can_compare(left, inner)
            }
        } else {
            left == right
        }
    }

    pub fn generic_to_concrete(self, map: &HashMap<Symbol, Type>) -> Type {
        self.transform(
            &|ty| match ty {
                Type::GenericParam(ref sym) => match map.get(sym) {
                    Some(t) if t != &Type::Unknown => t.clone(),
                    _ => ty,
                },
                other => other,
            },
            &|type_params| {
                type_params
                    .iter()
                    .filter(|s| !map.contains_key(*s))
                    .cloned()
                    .collect()
            },
        )
    }
}
