use crate::parser::ast::TypeAst;
use crate::scanner::{Span, Token};
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

fn missing_generics(
    type_name: &str,
    generics_expected: &[Symbol],
    generics_provided: &[Type],
    span: Span,
) -> Result<(), TypeCheckerError> {
    if generics_expected.len() > generics_provided.len() {
        Err(TypeCheckerError::MissingGeneric {
            ty_name: type_name.to_string(),
            generic_name: generics_expected[generics_provided.len()].to_string(),
            span,
        })
    } else if generics_provided.len() > generics_expected.len() {
        Err(TypeCheckerError::TooManyGenerics {
            span,
            found: generics_provided.len(),
            expected: generics_expected.len(),
            type_name: type_name.to_string(),
        })
    } else {
        Ok(())
    }
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

    pub fn from_identifier(
        name: &Token,
        type_system: &TypeSystem,
        generics: &[TypeAst],
    ) -> Result<Type, TypeCheckerError> {
        let span = name.span.merge(
            generics
                .last()
                .map(|t: &TypeAst| t.span())
                .unwrap_or(name.span),
        );

        let generics: Result<Vec<_>, TypeCheckerError> = generics
            .iter()
            .map(|e| Self::from_ast(e, type_system))
            .collect();

        let generics = generics?;

        let name = name.lexeme;

        let check_generic =
            |name, generic_params| missing_generics(name, generic_params, &generics, span);

        if name == "number" {
            Ok(Type::Number)
        } else if name == "string" {
            Ok(Type::String)
        } else if name == "boolean" {
            Ok(Type::Boolean)
        } else if name == "void" {
            Ok(Type::Void)
        } else if name == "never" {
            Ok(Type::Never)
        } else if name == "List" {
            check_generic(name, &["T".into()])?;
            Ok(Type::List(Box::new(generics[0].clone())))
        } else if name == "Map" {
            check_generic(name, &["K".into(), "V".into()])?;
            Ok(Type::Map(
                Box::new(generics[0].clone()),
                Box::new(generics[1].clone()),
            ))
        } else if let Some(struct_type) = type_system.get_struct(name) {
            check_generic(&struct_type.name, &struct_type.generic_params)?;
            Ok(Type::Struct(struct_type.name.clone(), Rc::new(generics)))
        } else if let Some(iface) = type_system.get_interface(name) {
            Ok(Type::Interface(iface.name.clone(), Rc::new(generics)))
        } else if let Some(enum_type) = type_system.get_enum(name) {
            check_generic(&enum_type.name, &enum_type.generic_params)?;
            Ok(Type::Enum(enum_type.name.clone(), Rc::new(generics)))
        } else if let type_name = name.into()
            && type_system.does_generic_exist(&type_name)
        {
            Ok(Type::GenericParam(type_name))
        } else {
            Err(TypeCheckerError::UndefinedType {
                name: name.to_string(),
                span,
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
            TypeAst::Named(name, generics) => Self::from_identifier(name, type_system, generics),
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
                    type_params: vec![],
                })))
            }
            TypeAst::Optional(inner) => {
                let inner_ty = Self::from_ast(inner, type_system)?;
                Ok(Type::Optional(Box::new(inner_ty)))
            }
            TypeAst::Infer => Ok(Type::Unknown),
        }
    }

    pub fn from_function_ast(
        type_ast: &TypeAst<'_>,
        type_system: &TypeSystem,
        type_params: Vec<Symbol>,
    ) -> Result<Type, TypeCheckerError> {
        let mut a = Self::from_ast(type_ast, type_system)?;
        if let Type::Function(func_type) = &mut a {
            let inner = Rc::get_mut(func_type).unwrap();
            inner.type_params = type_params;
        }
        Ok(a)
    }

    pub fn from_method_ast(
        type_ast: &TypeAst<'_>,
        self_type: &Token,
        self_generics: &[Token],
        type_system: &TypeSystem,
    ) -> Result<Type, TypeCheckerError> {
        match type_ast {
            TypeAst::Function {
                param_types,
                return_type,
            } => {
                let mut resolved_params = Vec::new();

                let is_instance_method = if let Some(TypeAst::Named(name, _)) = param_types.first()
                {
                    if name.lexeme == "Self" {
                        let self_ty = Type::from_identifier(
                            self_type,
                            type_system,
                            self_generics
                                .iter()
                                .map(|t| TypeAst::Named(t.clone(), vec![]))
                                .collect::<Vec<_>>()
                                .as_slice(),
                        )?;
                        resolved_params.push(("self".to_string(), self_ty));
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

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
                    type_params: type_system.get_active_generics(),
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

fn print_generics(args: &[Type], f: &mut Formatter<'_>) -> std::fmt::Result {
    if args.is_empty() {
        Ok(())
    } else {
        write!(f, "<")?;
        for (i, arg) in args.iter().enumerate() {
            write!(f, "{}", arg)?;
            if i != args.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ">")
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Error => write!(f, "Error"),
            Type::Infer(n) => write!(f, "T{}", n),
            Type::Metatype(name, generic_args) => {
                write!(f, "Type {}", name)?;
                print_generics(generic_args, f)
            }
            Type::GenericParam(name) => write!(f, "{}", name),
            Type::Number => write!(f, "number"),
            Type::Boolean => write!(f, "boolean"),
            Type::String => write!(f, "string"),
            Type::Void => write!(f, "void"),
            Type::Never => write!(f, "never"),
            Type::List(inner) => write!(f, "List<{}>", inner),
            Type::Map(key, value) => write!(f, "Map<{}, {}>", key, value),
            Type::Function(function_type) => {
                write!(
                    f,
                    "func({}) -> {}",
                    function_type
                        .params
                        .iter()
                        .map(|t| format!("{}", t.1))
                        .collect::<Vec<_>>()
                        .join(", "),
                    function_type.return_type
                )
            }
            Type::Unknown => write!(f, "unknown"),
            Type::Any => write!(f, "any"),
            Type::Struct(name, generic_args) => {
                write!(f, "struct {}", name,)?;
                print_generics(generic_args, f)
            }
            Type::Interface(name, generic_args) => {
                write!(f, "interface {}", name,)?;
                print_generics(generic_args, f)
            }

            Type::Enum(name, generic_args) => {
                write!(f, "enum {}", name,)?;
                print_generics(generic_args, f)
            }
            Type::Optional(inner) => write!(f, "{}?", inner),
            Type::Nil => write!(f, "Nil"),
            Type::Tuple(types) => {
                write!(
                    f,
                    "tuple({})",
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
