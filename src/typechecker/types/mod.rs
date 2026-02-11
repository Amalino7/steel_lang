mod display;

use crate::scanner::Span;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub is_static: bool,
    pub is_vararg: bool,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub type_params: Vec<Symbol>,
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
pub struct StructType {
    pub origin: Span,
    pub fields: HashMap<String, usize>,
    pub ordered_fields: Vec<(String, Type)>,
    pub name: Symbol,
    pub generic_params: Vec<Symbol>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct InterfaceType {
    pub methods: HashMap<String, (usize, Type)>,
    pub name: Symbol,
    pub origin: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumType {
    pub name: Symbol,
    pub origin: Span,
    pub variants: HashMap<Symbol, usize>,
    pub generic_params: Vec<Symbol>,
    pub ordered_variants: Vec<(Symbol, Type)>, // Void, one arg, tuple, struct
}

impl EnumType {
    pub fn get_variant(&self, name: &str) -> Option<(usize, Type)> {
        self.variants
            .get(name)
            .map(|idx| (*idx, self.ordered_variants[*idx].1.clone()))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TupleType {
    pub types: Vec<Type>,
}

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
    Interface(Symbol, GenericArgs),
    Enum(Symbol, GenericArgs),
    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Any,
}

impl Type {
    pub fn is_concrete(&self) -> bool {
        match self {
            Type::Error => true,
            Type::Infer(_) => false,
            Type::Optional(inner) => inner.is_concrete(),
            Type::Function(f) => {
                f.params.iter().all(|(_, t)| t.is_concrete()) && f.return_type.is_concrete()
            }
            Type::Nil | Type::Number | Type::String | Type::Boolean | Type::Void | Type::Never => {
                true
            }
            Type::Unknown => false,
            Type::Tuple(elements) => elements.types.iter().all(|t| t.is_concrete()),
            Type::GenericParam(_) => true,
            Type::Metatype(_, _) => false,
            Type::Struct(_, args) => args.iter().all(|t| t.is_concrete()),
            Type::Interface(_, args) => args.iter().all(|t| t.is_concrete()),
            Type::Enum(_, args) => args.iter().all(|t| t.is_concrete()),
            Type::Any => true,
            Type::List(inner) => inner.is_concrete(),
            Type::Map(key, value) => key.is_concrete() && value.is_concrete(),
        }
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
            Type::Interface(name, _) => Some(name),
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
            Type::Map(_, _) => Some("Map"),
        }
    }
    pub fn new_function(
        params: Vec<(String, Type)>,
        return_type: Type,
        type_params: Vec<Symbol>,
    ) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_static: true,
            is_vararg: false,
            params,
            return_type,
            type_params,
        }))
    }

    pub fn new_vararg(param_types: Vec<Type>, return_type: Type) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_static: true,
            is_vararg: true,
            params: param_types
                .into_iter()
                .map(|e| ("_".to_string(), e))
                .collect(),
            return_type,
            type_params: vec![],
        }))
    }

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

    pub fn generic_to_concrete(self, generics_map: &HashMap<Symbol, Type>) -> Type {
        match self {
            Type::Metatype(_, _) => self,
            Type::Nil
            | Type::Number
            | Type::String
            | Type::Boolean
            | Type::Void
            | Type::Unknown
            | Type::Never
            | Type::Infer(_)
            | Type::Error
            | Type::Any => self,
            Type::Optional(inner) => {
                Type::Optional(Box::new(Self::generic_to_concrete(*inner, generics_map)))
            }
            Type::List(inner) => {
                Type::List(Box::new(Self::generic_to_concrete(*inner, generics_map)))
            }
            Type::Map(key_type, value_type) => Type::Map(
                Box::new(Self::generic_to_concrete(*key_type, generics_map)),
                Box::new(Self::generic_to_concrete(*value_type, generics_map)),
            ),
            Type::Function(func_type) => {
                if func_type.is_vararg {
                    return Type::Function(func_type);
                }
                let params = func_type
                    .params
                    .iter()
                    .map(|(s, t)| (s.to_string(), t.clone().generic_to_concrete(generics_map)))
                    .collect();
                let return_type = func_type
                    .return_type
                    .clone()
                    .generic_to_concrete(generics_map);

                Type::new_function(
                    params,
                    return_type,
                    func_type
                        .type_params
                        .iter()
                        .filter(|s| !generics_map.contains_key(*s))
                        .cloned()
                        .collect(),
                )
            }
            Type::Tuple(types) => {
                let new_types = types
                    .types
                    .iter()
                    .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
                    .collect();
                Type::Tuple(Rc::new(TupleType { types: new_types }))
            }
            Type::GenericParam(generic) => {
                if let Some(new_type) = generics_map.get(&generic) {
                    if new_type == &Type::Unknown {
                        Type::GenericParam(generic)
                    } else {
                        new_type.clone()
                    }
                } else {
                    Type::GenericParam(generic)
                }
            }
            Type::Struct(name, args) => {
                let resolved_args = args
                    .iter()
                    .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
                    .collect();
                Type::Struct(name, Rc::new(resolved_args))
            }
            Type::Interface(name, args) => {
                let resolved_args = args
                    .iter()
                    .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
                    .collect();
                Type::Interface(name, Rc::new(resolved_args))
            }
            Type::Enum(name, args) => {
                let resolved_args = args
                    .iter()
                    .map(|t| Self::generic_to_concrete(t.clone(), generics_map))
                    .collect();
                Type::Enum(name, Rc::new(resolved_args))
            }
        }
    }
}
