use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Literal, TypeAst};
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::Symbol;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub is_static: bool,
    pub is_vararg: bool,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub fields: HashMap<String, (usize, Type)>,
    pub name: Symbol,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InterfaceType {
    pub methods: HashMap<String, (usize, Type)>,
    pub name: Symbol,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Boolean,
    Void,
    Function(Rc<FunctionType>),
    Unknown,
    Struct(Symbol),
    Interface(Symbol),
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
        } else {
            Err(TypeCheckerError::UndefinedType {
                name: name.to_string(),
                line,
            })
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
        }
    }

    pub fn new_function(param_types: Vec<Type>, return_type: Type) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_static: true,
            is_vararg: false,
            param_types,
            return_type,
        }))
    }

    pub fn new_vararg(param_types: Vec<Type>, return_type: Type) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_static: true,
            is_vararg: true,
            param_types,
            return_type,
        }))
    }

    pub fn from_ast(
        type_ast: &TypeAst<'_>,
        type_system: &TypeSystem,
    ) -> Result<Type, TypeCheckerError> {
        match type_ast {
            TypeAst::Named(name) => Self::from_identifier(name, type_system),
            TypeAst::Function {
                param_types,
                return_type,
            } => {
                let param_types = param_types
                    .iter()
                    .map(|e| Self::from_ast(e, type_system))
                    .collect::<Result<Vec<Type>, TypeCheckerError>>()?;

                Ok(Type::new_function(
                    param_types,
                    Self::from_ast(return_type, type_system)?,
                ))
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
                let first_param_type = param_types.first();
                let mut result_param_type = vec![];

                let is_instance_method = if let Some(TypeAst::Named(name)) = first_param_type {
                    name.lexeme == "Self"
                } else {
                    false
                };

                if is_instance_method {
                    result_param_type.push(Type::from_identifier(self_type, type_system)?);
                }

                for param_type in param_types.iter().skip(0 + is_instance_method as usize) {
                    result_param_type.push(Self::from_ast(param_type, type_system)?);
                }

                Ok(Type::Function(Rc::new(FunctionType {
                    is_static: !is_instance_method,
                    is_vararg: false,
                    return_type: Self::from_ast(return_type.as_ref(), type_system)?,
                    param_types: result_param_type,
                })))
            }
            _ => unreachable!(),
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
                        .param_types
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(", "),
                    function_type.return_type
                )
            }
            Type::Unknown => write!(f, "Unknown"),
            Type::Any => write!(f, "any"),
            Type::Struct(name) => write!(f, "struct {} ", name),
            Type::Interface(name) => write!(f, "interface {} ", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub ty: Type,
    pub kind: ExprKind,
    pub line: u32,
}
#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone)]
pub enum LogicalOp {
    Or,
    And,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Concat,
    // Can add more specific operators later
    Add,
    Subtract,
    Multiply,
    Divide,

    LessNumber,
    LessEqualNumber,
    GreaterNumber,
    GreaterEqualNumber,
    EqualEqualNumber,

    LessString,
    LessEqualString,
    GreaterString,
    GreaterEqualString,
    EqualEqualString,
    EqualEqual,
}
#[derive(Debug, Clone)]
pub enum ExprKind {
    Literal(Literal),
    GetVar(ResolvedVar),
    GetField {
        object: Box<TypedExpr>,
        index: u8,
    },
    SetField {
        object: Box<TypedExpr>,
        index: u8,
        value: Box<TypedExpr>,
    },
    StructInit {
        name: Box<str>,
        args: Vec<TypedExpr>,
    },

    InterfaceUpcast {
        expr: Box<TypedExpr>,
        vtable_idx: u32,
    },

    InterfaceMethodGet {
        object: Box<TypedExpr>,
        method_index: u8,
    },

    Unary {
        operator: UnaryOp,
        operand: Box<TypedExpr>,
    },
    Binary {
        left: Box<TypedExpr>,
        operator: BinaryOp,
        right: Box<TypedExpr>,
    },
    Logical {
        left: Box<TypedExpr>,
        operator: LogicalOp,
        right: Box<TypedExpr>,
    },
    Assign {
        target: ResolvedVar,
        value: Box<TypedExpr>,
    },
    MethodGet {
        object: Box<TypedExpr>,
        method: ResolvedVar,
    },
    Call {
        callee: Box<TypedExpr>, // 8 bytes
        arguments: Vec<TypedExpr>,
    },
}
#[derive(Debug)]
pub struct TypedStmt {
    pub kind: StmtKind,
    pub line: u32,
    pub type_info: Type,
}
#[derive(Debug)]
pub enum StmtKind {
    Impl {
        methods: Box<[TypedStmt]>, // optimizes memory layout
        vtables: Box<[Vec<ResolvedVar>]>,
    }, // Might add meta-information later
    StructDecl {},
    Global {
        global_count: u32,
        stmts: Vec<TypedStmt>,
    },
    Expression(TypedExpr),
    Return(TypedExpr),
    Let {
        target: ResolvedVar,
        value: TypedExpr,
    },
    Block {
        body: Vec<TypedStmt>,
        variable_count: u8,
    },
    If {
        condition: TypedExpr,
        then_branch: Box<TypedStmt>,
        else_branch: Option<Box<TypedStmt>>,
    },
    While {
        condition: TypedExpr,
        body: Box<TypedStmt>,
    },
    Function {
        target: ResolvedVar,
        name: Box<str>, // reduces memory usage by 8 bytes
        body: Box<TypedStmt>,
        captures: Box<[ResolvedVar]>,
    },
}
