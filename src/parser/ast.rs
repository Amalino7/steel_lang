use crate::scanner::{Span, Token};

#[derive(Clone, Debug, PartialEq)]
pub enum StringPart<'src> {
    Literal(String),
    Expr(Box<Expr<'src>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Void,
    Nil,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionSig<'src> {
    pub params: Box<[(Token<'src>, TypeAst<'src>)]>,
    pub return_type: Box<TypeAst<'src>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAst<'src> {
    Optional(Box<TypeAst<'src>>),
    Named(Token<'src>, Vec<TypeAst<'src>>),
    /// The span is the `func` keyword. The parameter names in the signature are placeholders.
    Function(Span, FunctionSig<'src>),
    Tuple(Vec<TypeAst<'src>>),
    Infer,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'src> {
    Error(Span),
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
    StringInterp {
        parts: Vec<StringPart<'src>>,
        span: Span,
    },
    Block {
        brace_token: Token<'src>,
        body: Vec<Stmt<'src>>,
        tail: Option<Box<Expr<'src>>>,
    },
    IfExpr {
        condition: Box<Expr<'src>>,
        then_branch: Box<Expr<'src>>,
        else_branch: Option<Box<Expr<'src>>>,
    },
    MatchExpr {
        value: Box<Expr<'src>>,
        arms: Vec<ExprMatchArm<'src>>,
    },
    Lambda {
        params: Vec<(Token<'src>, TypeAst<'src>)>,
        return_type: Option<TypeAst<'src>>,
        body: Box<Expr<'src>>,
        pipe_token: Token<'src>,
    },
    Return {
        keyword: Token<'src>,
        value: Box<Expr<'src>>,
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
pub struct ExprMatchArm<'src> {
    pub pattern: Pattern<'src>,
    pub body: Expr<'src>,
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

#[derive(Clone, Debug, PartialEq)]
pub struct StructField<'src> {
    pub is_public: bool,
    pub name: Token<'src>,
    pub type_info: TypeAst<'src>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    Let {
        is_public: bool,
        binding: Binding<'src>,
        value: Expr<'src>,
        type_info: TypeAst<'src>,
    },
    Block {
        open_brace: Token<'src>,
        body: Vec<Stmt<'src>>,
        close_brace: Token<'src>,
    },
    While {
        condition: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
    Function {
        is_public: bool,
        name: Token<'src>,
        body: Box<Stmt<'src>>,
        generics: Vec<Token<'src>>,
        signature: FunctionSig<'src>,
    },
    ExternFunction {
        is_public: bool,
        name: Token<'src>,
        generics: Vec<Token<'src>>,
        signature: FunctionSig<'src>,
    },
    Struct {
        is_public: bool,
        name: Token<'src>,
        fields: Vec<StructField<'src>>,
        generics: Vec<Token<'src>>,
    },
    Impl {
        interfaces: Vec<Token<'src>>,
        name: (Token<'src>, Vec<TypeAst<'src>>),
        methods: Vec<Stmt<'src>>,
        generics: Vec<Token<'src>>,
    },
    Interface {
        is_public: bool,
        name: Token<'src>,
        methods: Vec<MethodSig<'src>>,
        generics: Vec<Token<'src>>,
    },
    Enum {
        is_public: bool,
        name: Token<'src>,
        variants: Vec<(Token<'src>, VariantType<'src>)>,
        generics: Vec<Token<'src>>,
    },
}

/// A parsed source file: the import header followed by the declarations.
#[derive(Clone, Debug, PartialEq)]
pub struct ParsedFile<'src> {
    pub imports: Vec<ImportStmt<'src>>,
    pub body: Vec<Stmt<'src>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportStmt<'src> {
    pub keyword: Token<'src>,
    pub segment: ImportSegment<'src>,
    pub semicolon: Token<'src>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ImportSegment<'src> {
    pub path: Vec<Token<'src>>,
    pub import_type: ImportType<'src>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportType<'src> {
    Simple {
        terminator: Token<'src>,
    },
    Alias {
        terminator: Token<'src>,
        alias: Token<'src>,
    },
    Group {
        options: Vec<ImportSegment<'src>>,
    },
    Wildcard(Token<'src>),
}

impl ImportStmt<'_> {
    pub fn span(&self) -> Span {
        self.keyword.span.merge(self.semicolon.span)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum VariantType<'src> {
    Tuple(Vec<TypeAst<'src>>),
    Struct(Vec<StructField<'src>>),
    Unit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodSig<'src> {
    pub name: Token<'src>,
    pub signature: FunctionSig<'src>,
    pub generics: Vec<Token<'src>>,
}

impl Expr<'_> {
    pub fn span(&self) -> Span {
        match self {
            Expr::Error(span) => *span,
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
                .unwrap_or_default(),
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
            Expr::StringInterp { span, .. } => *span,
            Expr::Block {
                brace_token,
                body,
                tail,
            } => {
                let base = brace_token.span;
                if let Some(t) = tail {
                    base.merge(t.span())
                } else if let Some(last) = body.last() {
                    base.merge(last.span())
                } else {
                    base
                }
            }
            Expr::IfExpr {
                condition,
                then_branch,
                else_branch,
            } => {
                let base = condition.span().merge(then_branch.span());
                else_branch
                    .as_ref()
                    .map(|e| base.merge(e.span()))
                    .unwrap_or(base)
            }
            Expr::MatchExpr { value, arms } => arms
                .last()
                .map(|a| value.span().merge(a.body.span()))
                .unwrap_or_else(|| value.span()),
            Expr::Lambda {
                pipe_token, body, ..
            } => pipe_token.span.merge(body.span()),
            Expr::Return { keyword, value } => keyword.span.merge(value.span()),
        }
    }
}
impl Stmt<'_> {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Expression(expr) => expr.span(),
            Stmt::Let { binding, value, .. } => binding.span().merge(value.span()),
            Stmt::Block {
                open_brace,
                close_brace,
                ..
            } => open_brace.span.merge(close_brace.span),
            Stmt::While { condition, body } => condition.span().merge(body.span()),
            Stmt::Function { name, body, .. } => name.span.merge(body.span()),
            Stmt::ExternFunction {
                name, signature, ..
            } => name.span.merge(signature.return_type.span()),
            Stmt::Struct { name, fields, .. } => fields
                .last()
                .map(|field| name.span.merge(field.name.span))
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
                .unwrap_or_default(),
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
            TypeAst::Function(keyword, FunctionSig { return_type, .. }) => {
                keyword.merge(return_type.span())
            }
            TypeAst::Tuple(types) => types
                .last()
                .map(|last| types[0].span().merge(last.span()))
                .unwrap_or_default(),
            TypeAst::Infer => Span::default(),
        }
    }
}
