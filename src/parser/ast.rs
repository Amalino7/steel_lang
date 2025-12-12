use crate::token::Token;
use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Void,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAst<'src> {
    Named(Token<'src>),
    Function {
        param_types: Box<[TypeAst<'src>]>,
        return_type: Box<TypeAst<'src>>,
    },
    Infer,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
    Unary {
        operator: Token<'src>,
        expression: Box<Expr<'src>>,
    },
    StructInitializer {
        name: Token<'src>,
        fields: Vec<(Token<'src>, Expr<'src>)>,
    },
    Binary {
        operator: Token<'src>,
        left: Box<Expr<'src>>,
        right: Box<Expr<'src>>,
    },
    Variable {
        name: Token<'src>,
    },
    Grouping {
        expression: Box<Expr<'src>>,
    },
    Literal {
        literal: Literal,
        line: u32,
    },
    Assignment {
        identifier: Token<'src>,
        value: Box<Expr<'src>>,
    },
    Logical {
        left: Box<Expr<'src>>,
        operator: Token<'src>,
        right: Box<Expr<'src>>,
    },
    Call {
        callee: Box<Expr<'src>>,
        arguments: Vec<Expr<'src>>,
    },
    Get {
        object: Box<Expr<'src>>,
        field: Token<'src>,
    },
    Set {
        object: Box<Expr<'src>>,
        field: Token<'src>,
        value: Box<Expr<'src>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    Let {
        identifier: Token<'src>,
        value: Expr<'src>,
        type_info: TypeAst<'src>,
    },
    Return(Expr<'src>),
    Block {
        brace_token: Token<'src>,
        body: Vec<Stmt<'src>>,
    },
    If {
        condition: Expr<'src>,
        then_branch: Box<Stmt<'src>>,
        else_branch: Option<Box<Stmt<'src>>>,
    },
    While {
        condition: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
    Function {
        name: Token<'src>,
        params: Vec<Token<'src>>,
        body: Vec<Stmt<'src>>,
        type_: TypeAst<'src>,
    },
    Struct {
        name: Token<'src>,
        fields: Vec<(Token<'src>, TypeAst<'src>)>,
    },
    Impl {
        interfaces: Vec<Token<'src>>,
        name: Token<'src>,
        methods: Vec<Stmt<'src>>,
    },
    Interface {
        name: Token<'src>,
        methods: Vec<InterfaceSig<'src>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceSig<'src> {
    pub name: Token<'src>,
    pub params: Vec<Token<'src>>,
    pub type_: TypeAst<'src>,
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Void => write!(f, "void"),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Unary {
                operator,
                expression,
            } => {
                write!(f, "({} {})", operator.lexeme, expression)
            }
            Expr::Binary {
                operator,
                left,
                right,
            } => {
                write!(f, "({} {} {})", operator.lexeme, left, right)
            }
            Expr::Grouping { expression } => {
                write!(f, "(group {})", expression)
            }
            Expr::Literal { literal, .. } => {
                write!(f, "{}", literal)
            }
            Expr::Variable { name, .. } => write!(f, "{}", name.lexeme),
            Expr::Assignment {
                identifier, value, ..
            } => {
                write!(f, "{} = {}", identifier.lexeme, value)
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", operator.lexeme, left, right)
            }
            Expr::Call { callee, arguments } => {
                write!(
                    f,
                    "{}({})",
                    callee,
                    arguments
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(",")
                )
            }

            Expr::Get { object, field } => {
                write!(f, "{}.{}", object, field.lexeme)
            }
            Expr::Set {
                object,
                field: name,
                value,
            } => {
                write!(f, "{}.{} = {}", object, name.lexeme, value)
            }
            Expr::StructInitializer { name, fields } => {
                write!(f, "{} {{", name.lexeme)?;
                for (name, value) in fields {
                    write!(f, "{} : {},", name.lexeme, value)?;
                }
                write!(f, "}}")
            }
        }
    }
}
impl Display for TypeAst<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeAst::Named(name) => write!(f, "{}", name.lexeme),
            TypeAst::Function {
                param_types: params,
                return_type,
            } => {
                write!(
                    f,
                    "fn({}) -> {}",
                    params
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(", "),
                    return_type
                )
            }
            TypeAst::Infer => write!(f, "_"),
        }
    }
}

impl Display for Stmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "{}", expr),
            Stmt::Let {
                identifier,
                value,
                type_info,
                ..
            } => write!(f, "let {}: {} = {}", identifier.lexeme, type_info, value),
            Stmt::Block { body, .. } => {
                write!(f, "do\n")?;
                for statement in body {
                    write!(f, " {}\n", statement)?;
                }
                write!(f, "end")
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                if let Some(else_branch) = else_branch {
                    write!(
                        f,
                        "if {} then {} else {}",
                        condition, then_branch, else_branch
                    )
                } else {
                    write!(f, "if {} then {}", condition, then_branch)
                }
            }
            Stmt::While { condition, body } => write!(f, "while {} then {}", condition, body),
            Stmt::Function {
                name, params, body, ..
            } => {
                write!(
                    f,
                    "func {}({}) do {} end",
                    name.lexeme,
                    params
                        .iter()
                        .map(|e| e.lexeme)
                        .collect::<Vec<_>>()
                        .join(","),
                    body.iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            Stmt::Return(expr) => write!(f, "return {}", expr),

            Stmt::Struct { name, fields } => {
                write!(f, "Struct {} {{", name.lexeme)?;
                for field in fields {
                    write!(f, "{} : {}, ", field.0.lexeme, field.1)?;
                }
                write!(f, "}}")
            }
            Stmt::Impl {
                interfaces,
                name,
                methods,
            } => {
                write!(
                    f,
                    "Impl {} : {} {{",
                    interfaces
                        .iter()
                        .map(|e| e.lexeme)
                        .collect::<Vec<_>>()
                        .join(", "),
                    name.lexeme
                )?;
                for method in methods {
                    write!(f, "{}\n", method)?;
                }
                write!(f, "}}")
            }
            Stmt::Interface { name, methods } => {
                write!(f, "Interface {} {{", name.lexeme)?;
                for m in methods {
                    write!(f, " func {}(...): {};", m.name.lexeme, m.type_)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Expr<'_> {
    pub fn get_line(&self) -> u32 {
        match self {
            Expr::Unary { operator, .. } => operator.line,
            Expr::Binary { operator, .. } => operator.line,
            Expr::Variable { name, .. } => name.line,
            Expr::Grouping { expression } => expression.get_line(),
            Expr::Literal { line, .. } => *line,
            Expr::Assignment { identifier, .. } => identifier.line,
            Expr::Logical { operator, .. } => operator.line,
            Expr::Call { callee, .. } => callee.get_line(),
            Expr::Get { field, .. } => field.line,
            Expr::Set { value, .. } => value.get_line(),
            Expr::StructInitializer { name, .. } => name.line,
        }
    }
}

impl Stmt<'_> {
    pub fn get_line(&self) -> u32 {
        match self {
            Stmt::Expression(expr) => expr.get_line(),
            Stmt::Let { identifier, .. } => identifier.line,
            Stmt::Block { body, .. } => {
                if let Some(first_stmt) = body.first() {
                    first_stmt.get_line()
                } else {
                    0 //might need to add an alternative for empty blocks.
                }
            }
            Stmt::If { condition, .. } => condition.get_line(),
            Stmt::While { condition, .. } => condition.get_line(),
            Stmt::Function { name, .. } => name.line,
            Stmt::Return(expr) => expr.get_line(),
            Stmt::Struct { name, .. } => name.line,
            Stmt::Impl { name, .. } => name.line,
            Stmt::Interface { name, .. } => name.line,
        }
    }
}
