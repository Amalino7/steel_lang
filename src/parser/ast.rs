use crate::token::Token;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Void,
    Nil,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAst<'src> {
    Optional(Box<TypeAst<'src>>),
    Named(Token<'src>, Vec<TypeAst<'src>>),
    Function {
        param_types: Box<[TypeAst<'src>]>,
        return_type: Box<TypeAst<'src>>,
    },
    Tuple(Vec<TypeAst<'src>>),
    Infer,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
    Unary {
        operator: Token<'src>,
        expression: Box<Expr<'src>>,
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
    Is {
        expression: Box<Expr<'src>>,
        type_name: Token<'src>,
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
    TypeSpecialization {
        callee: Box<Expr<'src>>,
        generics: Vec<TypeAst<'src>>,
    },
    Call {
        callee: Box<Expr<'src>>,
        arguments: Vec<CallArg<'src>>,
        safe: bool,
    },
    Get {
        safe: bool,
        object: Box<Expr<'src>>,
        field: Token<'src>,
    },
    Set {
        safe: bool,
        object: Box<Expr<'src>>,
        field: Token<'src>,
        value: Box<Expr<'src>>,
    },
    Tuple {
        elements: Vec<Expr<'src>>,
    },
    ForceUnwrap {
        expression: Box<Expr<'src>>,
        line: u32,
    },
}
#[derive(Clone, Debug, PartialEq)]
pub struct CallArg<'src> {
    pub label: Option<Token<'src>>,
    pub expr: Expr<'src>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm<'src> {
    pub pattern: Pattern<'src>,
    pub body: Stmt<'src>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'src> {
    Variable(Token<'src>),
    Named {
        enum_name: Option<Token<'src>>,
        variant_name: Token<'src>,
        bind: Option<Binding<'src>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Binding<'src> {
    Struct {
        name: Token<'src>,
        fields: Vec<(Token<'src>, Binding<'src>)>,
    },
    Tuple {
        fields: Vec<Binding<'src>>,
    },
    Variable(Token<'src>),
}

impl<'src> Binding<'src> {
    pub(crate) fn get_line(&self) -> u32 {
        match self {
            Binding::Struct { name, .. } => name.line,
            Binding::Tuple { fields } => fields[0].get_line(),
            Binding::Variable(name) => name.line,
        }
    }
}
impl Display for Binding<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Binding::Struct { name, fields } => {
                write!(f, "{}", name.lexeme)?;
                write!(f, "(")?;
                for field in fields {
                    write!(f, " {}: {} ,", field.0.lexeme, field.1)?;
                }
                write!(f, ")")
            }
            Binding::Tuple { fields } => {
                write!(f, "(")?;
                for field in fields {
                    write!(f, " {} ,", field)?;
                }
                write!(f, ")")
            }
            Binding::Variable(name) => {
                write!(f, "{}", name.lexeme)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    Let {
        binding: Binding<'src>,
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
        generics: Vec<Token<'src>>,
        type_: TypeAst<'src>,
    },
    Struct {
        name: Token<'src>,
        fields: Vec<(Token<'src>, TypeAst<'src>)>,
        generics: Vec<Token<'src>>,
    },
    Impl {
        interfaces: Vec<Token<'src>>,
        name: (Token<'src>, Vec<Token<'src>>),
        methods: Vec<Stmt<'src>>,
        generics: Vec<Token<'src>>,
    },
    Interface {
        name: Token<'src>,
        methods: Vec<InterfaceSig<'src>>,
        generics: Vec<Token<'src>>,
    },
    Enum {
        name: Token<'src>,
        variants: Vec<(Token<'src>, VariantType<'src>)>,
        generics: Vec<Token<'src>>,
    },
    Match {
        value: Box<Expr<'src>>,
        arms: Vec<MatchArm<'src>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum VariantType<'src> {
    Tuple(Vec<TypeAst<'src>>),
    Struct(Vec<(Token<'src>, TypeAst<'src>)>),
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceSig<'src> {
    pub name: Token<'src>,
    pub params: Vec<Token<'src>>,
    pub type_: TypeAst<'src>,
    pub generics: Vec<Token<'src>>,
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Void => write!(f, "void"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

impl Display for Expr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Tuple { elements } => {
                write!(f, "(")?;
                for (i, element) in elements.iter().enumerate() {
                    write!(f, "{}", element)?;
                    if i != elements.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
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
            Expr::TypeSpecialization { callee, generics } => {
                write!(f, "{} <", callee)?;
                for generic in generics {
                    write!(f, "{}, ", generic)?;
                }
                write!(f, ">")
            }
            Expr::Call {
                callee, arguments, ..
            } => {
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

            Expr::Get {
                object,
                field,
                safe,
            } => {
                if *safe {
                    write!(f, "{}?.{}", object, field.lexeme)
                } else {
                    write!(f, "{}.{}", object, field.lexeme)
                }
            }
            Expr::Set {
                object,
                field: name,
                value,
                safe,
            } => {
                if *safe {
                    write!(f, "{}?.{} = {}", object, name.lexeme, value)
                } else {
                    write!(f, "{}.{} = {}", object, name.lexeme, value)
                }
            }
            Expr::Is {
                expression,
                type_name,
            } => write!(f, "{} is {}", expression, type_name.lexeme),

            Expr::ForceUnwrap { expression, .. } => {
                write!(f, "!!({})", expression)
            }
        }
    }
}
impl Display for CallArg<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.label {
            None => write!(f, "{}", self.expr),
            Some(label) => write!(f, "{} : {} ", label.lexeme, self.expr),
        }
    }
}
impl Display for TypeAst<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeAst::Named(name, _) => write!(f, "{}", name.lexeme),
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
            TypeAst::Optional(inner) => write!(f, "{}?", inner),
            TypeAst::Tuple(types) => {
                write!(
                    f,
                    "({})",
                    types
                        .iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

impl Display for Stmt<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "{}", expr),
            Stmt::Let {
                binding,
                value,
                type_info,
                ..
            } => write!(f, "let {}: {} = {}", binding, type_info, value),
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

            Stmt::Struct {
                name,
                fields,
                generics,
            } => {
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
                generics,
            } => {
                write!(
                    f,
                    "Impl {} : {} {{",
                    interfaces
                        .iter()
                        .map(|e| e.lexeme)
                        .collect::<Vec<_>>()
                        .join(", "),
                    name.0.lexeme
                )?;
                for method in methods {
                    write!(f, "{}\n", method)?;
                }
                write!(f, "}}")
            }
            Stmt::Interface {
                name,
                methods,
                generics,
            } => {
                write!(f, "Interface {} {{", name.lexeme)?;
                for m in methods {
                    write!(f, " func {}(...): {};", m.name.lexeme, m.type_)?;
                }
                write!(f, "}}")
            }
            Stmt::Enum {
                name,
                variants,
                generics,
            } => {
                write!(f, "Enum {} {{", name.lexeme)?;
                for case in variants {
                    write!(f, "case {} ", case.0.lexeme,)?;
                    match &case.1 {
                        VariantType::Tuple(els) => {
                            write!(f, "(")?;
                            for e in els {
                                write!(f, "{}, ", e)?;
                            }
                            write!(f, ")")?;
                        }
                        VariantType::Struct(str) => {
                            write!(f, "{{")?;
                            for (name, ty) in str {
                                write!(f, "{} : {},", name.lexeme, ty)?;
                            }
                            write!(f, "}}")?;
                        }
                        VariantType::Unit => {}
                    }
                    write!(f, ")")?;
                }
                write!(f, "}}")
            }
            Stmt::Match { value, arms } => {
                write!(f, "match {} {{", value)?;
                for arm in arms {
                    arm.pattern.fmt(f)?;
                    write!(f, " => {}", arm.body)?;
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
            Expr::ForceUnwrap { line, .. } => *line,
            Expr::Tuple { elements } => elements[0].get_line(),
            Expr::Is { type_name, .. } => type_name.line,
            Expr::TypeSpecialization { callee, .. } => callee.get_line(),
        }
    }
}

impl Stmt<'_> {
    pub fn get_line(&self) -> u32 {
        match self {
            Stmt::Expression(expr) => expr.get_line(),
            Stmt::Let {
                binding: identifier,
                ..
            } => identifier.get_line(),
            Stmt::Block { brace_token, .. } => brace_token.line,
            Stmt::If { condition, .. } => condition.get_line(),
            Stmt::While { condition, .. } => condition.get_line(),
            Stmt::Function { name, .. } => name.line,
            Stmt::Return(expr) => expr.get_line(),
            Stmt::Struct { name, .. } => name.line,
            Stmt::Impl { name, .. } => name.0.line,
            Stmt::Interface { name, .. } => name.line,
            Stmt::Enum { name, .. } => name.line,
            Stmt::Match { value, .. } => value.get_line(),
        }
    }
}
