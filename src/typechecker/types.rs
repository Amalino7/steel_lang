use crate::parser::ast::TypeAst;
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub is_static: bool,
    pub is_vararg: bool,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
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
    pub fields: HashMap<String, usize>,
    pub ordered_fields: Vec<(String, Type)>,
    pub name: Symbol,
}

impl StructType {
    pub fn get_field(&self, name: &str) -> Option<(usize, Type)> {
        self.fields
            .get(name)
            .map(|idx| (*idx, self.ordered_fields[*idx].1.clone()))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InterfaceType {
    pub methods: HashMap<String, (usize, Type)>,
    pub name: Symbol,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumType {
    pub name: Symbol,
    pub variants: HashMap<String, (usize, Type)>, // Void, one arg, tuple, struct
}
#[derive(Debug, PartialEq, Clone)]
pub struct TupleType {
    pub types: Vec<Type>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Nil,
    Number,
    String,
    Boolean,
    Void,
    Function(Rc<FunctionType>),
    Tuple(Rc<TupleType>),
    Unknown,
    Struct(Symbol),
    Interface(Symbol),
    Optional(Box<Type>),
    Enum(Symbol),
    Any, //TODO replace with generic types, this is for native functions.
}

impl Type {
    pub fn from_identifier(
        name: &Token,
        type_system: &TypeSystem,
    ) -> Result<Type, TypeCheckerError> {
        let line = name.line;
        let name = name.lexeme;
        if name == "number" {
            Ok(Type::Number)
        } else if name == "string" {
            Ok(Type::String)
        } else if name == "boolean" {
            Ok(Type::Boolean)
        } else if name == "void" {
            Ok(Type::Void)
        } else if let Some(struct_type) = type_system.get_struct(name) {
            Ok(Type::Struct(struct_type.name.clone()))
        } else if let Some(iface) = type_system.get_interface(name) {
            Ok(Type::Interface(iface.name.clone()))
        } else if let Some(enum_type) = type_system.get_enum(name) {
            Ok(Type::Enum(enum_type.name.clone()))
        } else {
            Err(TypeCheckerError::UndefinedType {
                name: name.to_string(),
                line,
                message: "Could not find type with that name.",
            })
        }
    }

    pub fn new_tuple(type_ast: &[TypeAst], sys: &TypeSystem) -> Result<Type, TypeCheckerError> {
        let mut types = vec![];
        for tuple_ty in type_ast {
            types.push(Type::from_ast(tuple_ty, sys)?);
        }
        if types.len() == 1 {
            Ok(types[0].clone())
        } else {
            Ok(Type::Tuple(Rc::from(TupleType { types })))
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
            Type::Interface(name) => Some(name),
            Type::Number => Some("number"),
            Type::String => Some("string"),
            Type::Boolean => Some("boolean"),
            Type::Void => Some("void"),
            Type::Function(_) => None,
            Type::Unknown => None,
            Type::Struct(name) => Some(name),
            Type::Any => None,
            Type::Optional(_) => None,
            Type::Nil => Some("nil"),
            Type::Enum(name) => Some(name),
            Type::Tuple(_) => None,
        }
    }
    pub fn new_function(params: Vec<(String, Type)>, return_type: Type) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_static: true,
            is_vararg: false,
            params,
            return_type,
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
        }))
    }

    pub fn from_ast(
        type_ast: &TypeAst<'_>,
        type_system: &TypeSystem,
    ) -> Result<Type, TypeCheckerError> {
        match type_ast {
            TypeAst::Tuple(types) => {
                let mut types_vec = Vec::with_capacity(types.len());
                for t in types {
                    types_vec.push(Self::from_ast(t, type_system)?);
                }
                Ok(Type::Tuple(Rc::new(TupleType { types: types_vec })))
            }
            TypeAst::Named(name) => Self::from_identifier(name, type_system),
            TypeAst::Function {
                param_types,
                return_type,
            } => {
                let mut params = Vec::with_capacity(param_types.len());

                for p_ast in param_types {
                    let ty = Self::from_ast(p_ast, type_system)?;
                    params.push(("_".to_string(), ty));
                }

                Ok(Type::Function(Rc::new(FunctionType {
                    is_static: false,
                    is_vararg: false,
                    params,
                    return_type: Self::from_ast(return_type, type_system)?,
                })))
            }
            TypeAst::Optional(inner) => {
                let inner_ty = Self::from_ast(inner, type_system)?;
                Ok(Type::Optional(Box::new(inner_ty)))
            }
            TypeAst::Infer => Ok(Type::Unknown),
        }
    }

    pub fn from_method_ast(
        type_ast: &TypeAst<'_>,
        self_type: &Token,
        type_system: &TypeSystem,
    ) -> Result<Type, TypeCheckerError> {
        match type_ast {
            TypeAst::Function {
                param_types,
                return_type,
            } => {
                let mut resolved_params = Vec::new();

                let is_instance_method = if let Some(TypeAst::Named(name)) = param_types.first() {
                    name.lexeme == "Self"
                } else {
                    false
                };

                if is_instance_method {
                    let self_ty = Type::from_identifier(self_type, type_system)?;
                    resolved_params.push(("self".to_string(), self_ty));
                }

                for param_ast in param_types
                    .iter()
                    .skip(if is_instance_method { 1 } else { 0 })
                {
                    let ty = Self::from_ast(param_ast, type_system)?;
                    resolved_params.push(("_".to_string(), ty));
                }

                Ok(Type::Function(Rc::new(FunctionType {
                    is_static: !is_instance_method,
                    is_vararg: false,
                    return_type: Self::from_ast(return_type.as_ref(), type_system)?,
                    params: resolved_params,
                })))
            }
            _ => unreachable!(),
        }
    }

    pub fn patch_param_names(self, params: &[Token]) -> Type {
        if let Type::Function(mut func_rc) = self {
            let func = Rc::make_mut(&mut func_rc);

            for (i, param_token) in params.iter().enumerate() {
                func.params[i].0 = param_token.lexeme.to_string();
            }

            Type::Function(func_rc)
        } else {
            unreachable!("Type::from_ast returned non-function for Function AST");
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Number => write!(f, "Number"),
            Type::Boolean => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::Void => write!(f, "Void"),
            Type::Function(function_type) => {
                write!(
                    f,
                    "fn({}) -> {}",
                    function_type
                        .params
                        .iter()
                        .map(|t| format!("{}", t.1))
                        .collect::<Vec<_>>()
                        .join(", "),
                    function_type.return_type
                )
            }
            Type::Unknown => write!(f, "Unknown"),
            Type::Any => write!(f, "any"),
            Type::Struct(name) => write!(f, "struct {} ", name),
            Type::Interface(name) => write!(f, "interface {} ", name),
            Type::Optional(inner) => write!(f, "Optional<{}>", inner),
            Type::Nil => write!(f, "Nil"),
            Type::Enum(name) => write!(f, "enum {} ", name),
            Type::Tuple(types) => {
                write!(
                    f,
                    "Tuple({})",
                    types
                        .types
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}
