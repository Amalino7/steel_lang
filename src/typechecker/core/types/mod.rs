mod display;
pub mod type_defs;

use crate::scanner::Span;
use crate::typechecker::core::error::TypeCheckerWarning;
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
pub type GenericArgs = Rc<[Type]>;

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
    Any,
}

impl Type {
    pub fn new_list(element: Type) -> Type {
        Type::Struct("List".into(), Rc::from(vec![element]))
    }

    pub fn new_map(key: Type, value: Type) -> Type {
        Type::Struct("Map".into(), Rc::from(vec![key, value]))
    }

    pub fn list_element(&self) -> Option<&Type> {
        let Type::Struct(name, args) = self else {
            return None;
        };
        if name.as_ref() == "List" {
            args.first()
        } else {
            None
        }
    }

    pub fn map_key(&self) -> Option<&Type> {
        let Type::Struct(name, args) = self else {
            return None;
        };
        if name.as_ref() == "Map" {
            args.first()
        } else {
            None
        }
    }

    pub fn map_value(&self) -> Option<&Type> {
        let Type::Struct(name, args) = self else {
            return None;
        };
        if name.as_ref() == "Map" {
            args.get(1)
        } else {
            None
        }
    }

    pub fn generic_args(&self) -> &[Type] {
        match self {
            Type::Struct(_, args) | Type::Enum(_, args) => args,
            _ => &[],
        }
    }

    pub fn generic_count(&self) -> usize {
        self.generic_args().len()
    }
}

impl Type {
    pub fn transform(
        &self,
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
            | Type::Interface(_) => self.clone(),

            Type::Optional(inner) => Type::Optional(Box::new(inner.transform(f, u))),
            Type::Tuple(tt) => {
                let types: Vec<_> = tt.types.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Tuple(Rc::from(TupleType { types }))
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
                let args: Vec<_> = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Struct(name.clone(), Rc::from(args))
            }
            Type::Enum(name, args) => {
                let args: Vec<_> = args.iter().map(|t| t.clone().transform(f, u)).collect();
                Type::Enum(name.clone(), Rc::from(args))
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

    /// Unwraps optional for safe access, emitting a warning instead if used on non-optional
    /// Returns also whether this is should be a safe access or a normal one.
    pub fn unwrap_optional_safe(
        &self,
        safe: bool,
        span: Span,
        warnings: &mut Vec<TypeCheckerWarning>,
    ) -> (Type, bool) {
        if safe {
            match self {
                Type::Optional(inner) => (inner.as_ref().clone(), true),
                ty => {
                    warnings.push(TypeCheckerWarning::SafeAccessOnNonOptional { span });
                    (ty.clone(), false)
                }
            }
        } else {
            (self.clone(), false)
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
        match (left, right) {
            (Type::Optional(_), Type::Nil) | (Type::Nil, Type::Optional(_)) => true,
            (Type::Optional(inner_l), Type::Optional(inner_r)) => {
                Self::can_compare(inner_l, inner_r)
            }
            (Type::Optional(inner), other) | (other, Type::Optional(inner)) => {
                Self::can_compare(inner, other)
            }
            (a, b) if a == b => true,
            _ => false,
        }
    }

    pub fn generic_to_concrete(&self, map: &HashMap<Symbol, Type>) -> Type {
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
