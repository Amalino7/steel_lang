use crate::scanner::{Span, Token};
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
        span: Span,
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
    List {
        elements: Vec<Expr<'src>>,
        bracket_token: Token<'src>,
    },
    Map {
        kv_pairs: Vec<(Expr<'src>, Expr<'src>)>,
        bracket_token: Token<'src>,
    },
    GetIndex {
        safe: bool,
        object: Box<Expr<'src>>,
        index: Box<Expr<'src>>,
    },
    SetIndex {
        safe: bool,
        object: Box<Expr<'src>>,
        index: Box<Expr<'src>>,
        value: Box<Expr<'src>>,
    },
    ForceUnwrap {
        expression: Box<Expr<'src>>,
        operator: Token<'src>,
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
            Expr::List { elements, .. } => {
                write!(f, "[")?;
                for (i, el) in elements.iter().enumerate() {
                    write!(f, "{}", el)?;
                    if i < elements.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Expr::Map { kv_pairs, .. } => {
                write!(f, "[")?;
                for (i, (k, v)) in kv_pairs.iter().enumerate() {
                    write!(f, "{}: {}", k, v)?;
                    if i < kv_pairs.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Expr::GetIndex { object, index, .. } => {
                write!(f, "{}[{}]", object, index)
            }
            Expr::SetIndex {
                object,
                index,
                value,
                ..
            } => write!(f, "{}[{}] = {}", object, index, value),
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
fn print_generics(f: &mut Formatter<'_>, generics: &[Token<'_>]) -> fmt::Result {
    write!(f, "<")?;
    for (i, generic) in generics.iter().enumerate() {
        write!(f, "{}", generic.lexeme)?;
        if i != generics.len() - 1 {
            write!(f, ", ")?;
        }
    }
    write!(f, ">")
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
                writeln!(f, "do\n")?;
                for statement in body {
                    writeln!(f, " {}\n", statement)?;
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
                print_generics(f, generics)?;
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
                write!(f, "Impl {}", name.0.lexeme)?;
                print_generics(f, generics)?;
                write!(
                    f,
                    ": {} {{",
                    interfaces
                        .iter()
                        .map(|e| e.lexeme)
                        .collect::<Vec<_>>()
                        .join(", "),
                )?;
                for method in methods {
                    writeln!(f, "{}\n", method)?;
                }
                write!(f, "}}")
            }
            Stmt::Interface {
                name,
                methods,
                generics,
            } => {
                write!(f, "Interface {}", name.lexeme)?;
                print_generics(f, generics)?;
                write!(f, ": {{")?;
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
                write!(f, "Enum {}", name.lexeme)?;
                print_generics(f, generics)?;
                write!(f, "{{")?;
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
    pub fn span(&self) -> Span {
        match self {
            Expr::Unary {
                operator,
                expression,
            } => operator.span.merge(expression.span()),
            Expr::Binary { left, right, .. } => left.span().merge(right.span()),
            Expr::Variable { name } => name.span,
            Expr::Grouping { expression } => expression.span(),
            Expr::Literal { span, .. } => *span,
            Expr::Assignment { identifier, value } => identifier.span.merge(value.span()),
            Expr::Logical { left, right, .. } => left.span().merge(right.span()),
            Expr::Call {
                callee, arguments, ..
            } => arguments
                .last()
                .map(|arg| callee.span().merge(arg.expr.span()))
                .unwrap_or_else(|| callee.span()),
            Expr::Get { object, field, .. } => object.span().merge(field.span),
            Expr::Set { object, value, .. } => object.span().merge(value.span()),
            Expr::Is {
                expression,
                type_name,
            } => expression.span().merge(type_name.span),
            Expr::TypeSpecialization { callee, generics } => generics
                .last()
                .map(|generic| callee.span().merge(generic.span()))
                .unwrap_or_else(|| callee.span()),
            Expr::Tuple { elements } => elements
                .last()
                .map(|last| elements[0].span().merge(last.span()))
                .unwrap_or_else(|| Span::default()),
            Expr::List {
                elements,
                bracket_token,
            } => elements
                .last()
                .map(|last| bracket_token.span.merge(last.span()))
                .unwrap_or(bracket_token.span),
            Expr::Map {
                kv_pairs,
                bracket_token,
            } => kv_pairs
                .last()
                .map(|(_, value)| bracket_token.span.merge(value.span()))
                .unwrap_or(bracket_token.span),
            Expr::GetIndex { object, index, .. } => object.span().merge(index.span()),
            Expr::SetIndex { object, value, .. } => object.span().merge(value.span()),
            Expr::ForceUnwrap { operator, .. } => operator.span,
        }
    }
}
impl Stmt<'_> {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Expression(expr) => expr.span(),
            Stmt::Let { binding, value, .. } => binding.span().merge(value.span()),
            Stmt::Return(expr) => expr.span(),
            Stmt::Block { body, brace_token } => body
                .last()
                .map(|last| brace_token.span.merge(last.span()))
                .unwrap_or(brace_token.span),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let base = condition.span().merge(then_branch.span());
                else_branch
                    .as_ref()
                    .map(|branch| base.merge(branch.span()))
                    .unwrap_or(base)
            }
            Stmt::While { condition, body } => condition.span().merge(body.span()),
            Stmt::Function { name, body, .. } => body
                .last()
                .map(|last| name.span.merge(last.span()))
                .unwrap_or(name.span),
            Stmt::Struct { name, fields, .. } => fields
                .last()
                .map(|(field, _)| name.span.merge(field.span))
                .unwrap_or(name.span),
            Stmt::Impl { name, methods, .. } => methods
                .last()
                .map(|last| name.0.span.merge(last.span()))
                .unwrap_or(name.0.span),
            Stmt::Interface { name, methods, .. } => methods
                .last()
                .map(|last| name.span.merge(last.name.span))
                .unwrap_or(name.span),
            Stmt::Enum { name, variants, .. } => variants
                .last()
                .map(|(variant, _)| name.span.merge(variant.span))
                .unwrap_or(name.span),
            Stmt::Match { value, arms } => arms
                .last()
                .map(|arm| value.span().merge(arm.body.span()))
                .unwrap_or_else(|| value.span()),
        }
    }
}
impl Binding<'_> {
    pub fn span(&self) -> Span {
        match self {
            Binding::Struct { name, fields } => fields
                .last()
                .map(|(_, binding)| name.span.merge(binding.span()))
                .unwrap_or(name.span),
            Binding::Tuple { fields } => fields
                .last()
                .map(|last| fields[0].span().merge(last.span()))
                .unwrap_or_else(|| Span::default()),
            Binding::Variable(name) => name.span,
        }
    }
}

impl TypeAst<'_> {
    pub fn span(&self) -> Span {
        match self {
            TypeAst::Optional(inner) => inner.span(),
            TypeAst::Named(name, generics) => generics
                .last()
                .map(|generic| name.span.merge(generic.span()))
                .unwrap_or(name.span),
            TypeAst::Function {
                param_types,
                return_type,
            } => param_types
                .last()
                .map(|last| param_types[0].span().merge(last.span()))
                .unwrap_or_else(|| return_type.span()),
            TypeAst::Tuple(types) => types
                .last()
                .map(|last| types[0].span().merge(last.span()))
                .unwrap_or_else(|| Span::default()),
            TypeAst::Infer => Span::default(),
        }
    }
}
