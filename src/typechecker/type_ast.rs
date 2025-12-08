use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::{Literal, TypeAst};
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionType {
    pub is_vararg: bool,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub fields: HashMap<String, (usize, Type)>,
    pub name: String,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Boolean,
    Void,
    Function(Rc<FunctionType>),
    Unknown,
    Struct(Rc<StructType>),
    Any, //TODO replace with generic types, this is for native functions.
}

impl Type {
    pub fn from_identifier(
        name: &Token,
        structs: &HashMap<&'_ str, Rc<StructType>>,
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
        } else {
            Ok(Type::Struct(Rc::clone(structs.get(name).ok_or(
                TypeCheckerError::UndefinedType {
                    name: name.to_string(),
                    line,
                },
            )?)))
        }
    }

    pub fn new_function(param_types: Vec<Type>, return_type: Type) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_vararg: false,
            param_types,
            return_type,
        }))
    }

    pub fn new_vararg(param_types: Vec<Type>, return_type: Type) -> Type {
        Type::Function(Rc::new(FunctionType {
            is_vararg: true,
            param_types,
            return_type,
        }))
    }

    pub fn from_ast(
        type_ast: &TypeAst<'_>,
        structs: &HashMap<&'_ str, Rc<StructType>>,
    ) -> Result<Type, TypeCheckerError> {
        match type_ast {
            TypeAst::Named(name) => Self::from_identifier(name, structs),
            TypeAst::Function {
                param_types,
                return_type,
            } => {
                let param_types = param_types
                    .iter()
                    .map(|e| Self::from_ast(e, structs))
                    .collect::<Result<Vec<Type>, TypeCheckerError>>()?;

                Ok(Type::new_function(
                    param_types,
                    Self::from_ast(return_type, structs)?,
                ))
            }
            TypeAst::Infer => Ok(Type::Unknown),
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
            Type::Struct(struct_type) => {
                write!(f, "struct {} ", struct_type.name,)?;
                write!(
                    f,
                    "{{ {} }}",
                    struct_type
                        .fields
                        .keys()
                        .map(|k| format!("{}: {}", k, struct_type.fields[k].1))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}
#[derive(Debug)]
pub struct TypedExpr {
    pub ty: Type,
    pub kind: ExprKind,
    pub line: u32,
}
#[derive(Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug)]
pub enum LogicalOp {
    Or,
    And,
}

#[derive(Debug)]
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
#[derive(Debug)]
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
