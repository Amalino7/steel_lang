//! The untyped AST: what the parser produces and the resolver (imports) and typechecker (everything else) read.
//!
//! Rules every node follows:
//! - **Spans are stored, never guessed.** Each node records the full source text it was parsed from, keywords,
//!   delimiters and terminators included (`let x = 1;` covers the `let` and the `;`). Expressions and types are
//!   `{ kind, span }`; statements, declarations and their parts carry a `span` field.
//! - **Fields are in source order**, broadest first: `vis, name, generics, params, ret, body`.
//! - **No tokens.** Names are [`Ident`], operators [`Spanned`] enums; later phases never see `TokenType`.
//! - **No placeholders.** An omitted annotation is `None`, `_` is `Wildcard`, a bare `return` has no value.
//!
//! The span to use in a diagnostic depends on what it is about (see `docs/features/parser-improvements`):
//! a name → `ident.span`; a value's type → `expr.span`; arguments → `args.span()` (or `args.close` for a
//! missing one); a declaration as context → `header_span()`; an operator → `op.span`.

use crate::scanner::Span;

/// A name as written in the source: `x`, `Point`, `self`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Ident<'src> {
    pub name: &'src str,
    pub span: Span,
}

/// A value with the span of the source text it came from, for operators.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

/// A delimited list, `( … )`, `< … >` or `| … |`, with the spans of its delimiters. `close` is a zero-width
/// point after the last token when the closer was missing (which is then a reported error).
#[derive(Clone, Debug, PartialEq)]
pub struct Delimited<T> {
    pub open: Span,
    pub items: Vec<T>,
    pub close: Span,
}

impl<T> Delimited<T> {
    /// From the opener through the closer.
    pub fn span(&self) -> Span {
        self.open.merge(self.close)
    }
}

/// `public`, or nothing. `Public` keeps the keyword's span for visibility diagnostics.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Visibility {
    Private,
    Public(Span),
}

impl Visibility {
    pub fn is_public(self) -> bool {
        matches!(self, Visibility::Public(_))
    }
}

// ---------------------------------------------------------------------------------------------------------------
// Files and imports
// ---------------------------------------------------------------------------------------------------------------

/// A parsed source file: the import header, then the top-level statements and declarations.
#[derive(Clone, Debug, PartialEq)]
pub struct ParsedFile<'src> {
    pub imports: Vec<Import<'src>>,
    pub stmts: Vec<Stmt<'src>>,
}

/// `import math/vector/{Vec2, add as plus};`
#[derive(Clone, Debug, PartialEq)]
pub struct Import<'src> {
    pub tree: ImportTree<'src>,
    /// From `import` through `;`.
    pub span: Span,
}

/// One path of an import: `math/vector/{…}`, or `add as plus` inside a group.
#[derive(Clone, Debug, PartialEq)]
pub struct ImportTree<'src> {
    /// The module segments before the last `/`. Empty only for an item inside a group.
    pub path: Vec<Ident<'src>>,
    pub kind: ImportKind<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ImportKind<'src> {
    /// `module/name`
    Item { name: Ident<'src> },
    /// `module/name as alias`
    Alias {
        name: Ident<'src>,
        alias: Ident<'src>,
    },
    /// `module/{a, b as c, sub/d}`. Never empty.
    Group(Delimited<ImportTree<'src>>),
    /// `module/{*}`, with the span of the `*`.
    Wildcard(Span),
}

// ---------------------------------------------------------------------------------------------------------------
// Statements and declarations
// ---------------------------------------------------------------------------------------------------------------

/// A statement in a block or at top level. Declarations are statements: whether one is allowed where it
/// appears (`struct` inside a function) is the typechecker's decision. The declarations are boxed, so a
/// statement stays as small as an expression.
#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'src> {
    /// `f(x);`, or a block-like expression without `;` (`if c { … }`). The span is the expression's.
    Expr(Expr<'src>),
    Let(Box<LetDecl<'src>>),
    While(Box<While<'src>>),
    Fn(Box<FnDecl<'src>>),
    Struct(Box<StructDecl<'src>>),
    Enum(Box<EnumDecl<'src>>),
    Interface(Box<InterfaceDecl<'src>>),
    Impl(Box<ImplDecl<'src>>),
}

impl Stmt<'_> {
    pub fn span(&self) -> Span {
        match self {
            Stmt::Expr(expr) => expr.span,
            Stmt::Let(decl) => decl.span,
            Stmt::While(stmt) => stmt.span,
            Stmt::Fn(decl) => decl.span,
            Stmt::Struct(decl) => decl.span,
            Stmt::Enum(decl) => decl.span,
            Stmt::Interface(decl) => decl.span,
            Stmt::Impl(decl) => decl.span,
        }
    }
}

/// `{ stmt* }`: a function or `while` body. Unlike a block *expression* it has no value, so it can't end in a
/// tail expression.
#[derive(Clone, Debug, PartialEq)]
pub struct Block<'src> {
    pub stmts: Vec<Stmt<'src>>,
    /// From `{` through `}`.
    pub span: Span,
}

/// `public let (x, y): (number, number) = point;`
#[derive(Clone, Debug, PartialEq)]
pub struct LetDecl<'src> {
    pub vis: Visibility,
    pub binding: Binding<'src>,
    /// `None` when omitted: the type is inferred from `value`.
    pub ty: Option<TypeAst<'src>>,
    pub value: Expr<'src>,
    /// From `public`/`let` through `;`.
    pub span: Span,
}

/// `while i < 10 { … }`
#[derive(Clone, Debug, PartialEq)]
pub struct While<'src> {
    pub condition: Expr<'src>,
    pub body: Block<'src>,
    pub span: Span,
}

/// `public func max<T>(a: T, b: T): T { … }`, or without a body, `extern func print(value: string);`.
#[derive(Clone, Debug, PartialEq)]
pub struct FnDecl<'src> {
    pub vis: Visibility,
    pub sig: FnSig<'src>,
    /// `None` exactly for `extern func`, which is implemented natively.
    pub body: Option<Block<'src>>,
    /// From `public`/`extern`/`func` through the body's `}`, or the `;` of an extern.
    pub span: Span,
}

impl FnDecl<'_> {
    pub fn is_extern(&self) -> bool {
        self.body.is_none()
    }

    /// From the first keyword through the signature: `public func max<T>(a: T, b: T): T`.
    pub fn header_span(&self) -> Span {
        self.span.until(self.sig.end())
    }
}

/// `name<G>(params): Ret`, shared by functions, methods and interface method signatures.
#[derive(Clone, Debug, PartialEq)]
pub struct FnSig<'src> {
    pub name: Ident<'src>,
    pub generics: Generics<'src>,
    pub params: Delimited<Param<'src>>,
    /// `None` when omitted, which means `void`.
    pub ret: Option<TypeAst<'src>>,
}

impl FnSig<'_> {
    /// The end of the signature: the return type, or the `)` without one.
    pub fn end(&self) -> Span {
        self.ret.as_ref().map_or(self.params.close, |ret| ret.span)
    }
}

/// A function parameter.
#[derive(Clone, Debug, PartialEq)]
pub enum Param<'src> {
    /// `self`: only in a method, and only as the first parameter. Its type is the `impl` target.
    SelfParam(Span),
    /// `name: Type`
    Named {
        name: Ident<'src>,
        ty: TypeAst<'src>,
    },
}

impl Param<'_> {
    pub fn span(&self) -> Span {
        match self {
            Param::SelfParam(span) => *span,
            Param::Named { name, ty } => name.span.merge(ty.span),
        }
    }

    /// The name the parameter binds: `self`, or its own.
    pub fn ident(&self) -> Ident<'_> {
        match self {
            Param::SelfParam(span) => Ident {
                name: "self",
                span: *span,
            },
            Param::Named { name, .. } => *name,
        }
    }
}

/// The generic parameters of a declaration, `<T, U>`, or nothing.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct Generics<'src>(pub Option<Delimited<Ident<'src>>>);

impl<'src> Generics<'src> {
    pub fn params(&self) -> &[Ident<'src>] {
        self.0.as_ref().map_or(&[], |list| &list.items)
    }

    /// From `<` through `>`, if present.
    pub fn span(&self) -> Option<Span> {
        self.0.as_ref().map(Delimited::span)
    }
}

/// `public struct Point<T> { public x: T, y: T }`
#[derive(Clone, Debug, PartialEq)]
pub struct StructDecl<'src> {
    pub vis: Visibility,
    pub name: Ident<'src>,
    pub generics: Generics<'src>,
    pub fields: Vec<FieldDecl<'src>>,
    pub span: Span,
}

impl StructDecl<'_> {
    /// `public struct Point<T>`
    pub fn header_span(&self) -> Span {
        header(self.span, self.name, &self.generics)
    }
}

/// `public x: number`, in a struct or a struct-like enum variant.
#[derive(Clone, Debug, PartialEq)]
pub struct FieldDecl<'src> {
    /// Always `Private` in an enum variant, whose fields are as visible as the enum.
    pub vis: Visibility,
    pub name: Ident<'src>,
    pub ty: TypeAst<'src>,
    pub span: Span,
}

/// `public enum Shape<T> { Empty, Circle(number), Rect { w: number, h: number } }`
#[derive(Clone, Debug, PartialEq)]
pub struct EnumDecl<'src> {
    pub vis: Visibility,
    pub name: Ident<'src>,
    pub generics: Generics<'src>,
    pub variants: Vec<Variant<'src>>,
    pub span: Span,
}

impl EnumDecl<'_> {
    /// `public enum Shape<T>`
    pub fn header_span(&self) -> Span {
        header(self.span, self.name, &self.generics)
    }
}

/// `Circle(number)`
#[derive(Clone, Debug, PartialEq)]
pub struct Variant<'src> {
    pub name: Ident<'src>,
    pub payload: VariantPayload<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum VariantPayload<'src> {
    /// `Empty`
    Unit,
    /// `Circle(number)`, `Pair(number, string)`. Never empty.
    Tuple(Vec<TypeAst<'src>>),
    /// `Rect { w: number, h: number }`
    Struct(Vec<FieldDecl<'src>>),
}

/// `public interface Show<T> { func show(self): string; }`
#[derive(Clone, Debug, PartialEq)]
pub struct InterfaceDecl<'src> {
    pub vis: Visibility,
    pub name: Ident<'src>,
    pub generics: Generics<'src>,
    pub methods: Vec<MethodSig<'src>>,
    pub span: Span,
}

impl InterfaceDecl<'_> {
    /// `public interface Show<T>`
    pub fn header_span(&self) -> Span {
        header(self.span, self.name, &self.generics)
    }
}

/// `func show(self): string;` inside an interface.
#[derive(Clone, Debug, PartialEq)]
pub struct MethodSig<'src> {
    pub sig: FnSig<'src>,
    /// From `func` through `;`.
    pub span: Span,
}

/// `impl<T> List<T>: Show, Eq { … }`. `impl` itself is never `public`; its methods are, one by one.
#[derive(Clone, Debug, PartialEq)]
pub struct ImplDecl<'src> {
    pub generics: Generics<'src>,
    pub target: NamedType<'src>,
    pub interfaces: Vec<NamedType<'src>>,
    pub methods: Vec<FnDecl<'src>>,
    pub span: Span,
}

impl ImplDecl<'_> {
    /// `impl<T> List<T>: Show, Eq`
    pub fn header_span(&self) -> Span {
        let end = self.interfaces.last().unwrap_or(&self.target).span;
        self.span.until(end)
    }
}

/// From the start of `span` through the name and generics.
fn header(span: Span, name: Ident, generics: &Generics) -> Span {
    span.until(generics.span().unwrap_or(name.span))
}

// ---------------------------------------------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------------------------------------------

/// A type as written: `number`, `List<T>?`, `(number, string)`, `func(number): bool`.
#[derive(Clone, Debug, PartialEq)]
pub struct TypeAst<'src> {
    pub kind: TypeAstKind<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeAstKind<'src> {
    /// `number`, `Map<string, T>`
    Named(NamedType<'src>),
    /// `T?`
    Optional(Box<TypeAst<'src>>),
    /// `(number, string)`, or the 1-tuple `(number,)`. Never empty.
    Tuple(Vec<TypeAst<'src>>),
    /// `func(number, string): bool`. A `func?(…)` is an `Optional` around one.
    Fn {
        params: Vec<TypeAst<'src>>,
        /// `None` when omitted, which means `void`.
        ret: Option<Box<TypeAst<'src>>>,
    },
}

/// A type name with optional type arguments: `List<T>`. Also the target and interfaces of an `impl`.
#[derive(Clone, Debug, PartialEq)]
pub struct NamedType<'src> {
    pub name: Ident<'src>,
    /// Empty without `<…>`.
    pub args: Vec<TypeAst<'src>>,
    pub span: Span,
}

// ---------------------------------------------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq)]
pub struct Expr<'src> {
    pub kind: ExprKind<'src>,
    pub span: Span,
}

impl<'src> Expr<'src> {
    pub fn new(kind: ExprKind<'src>, span: Span) -> Self {
        Expr { kind, span }
    }

    pub(crate) fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind<'src> {
    /// A malformed expression the parser already reported, kept so parsing can go on.
    Error,

    // Atoms
    /// `1`, `"text"`, `true`, `nil`
    Literal(Literal),
    /// `"x is ${x}"`. Literal parts are never empty strings.
    StringInterp(Vec<StringPart<'src>>),
    /// `x`, `self`, or a type name used as a value (`Point`).
    Variable(Ident<'src>),
    /// `(a + b)`
    Grouping(Box<Expr<'src>>),
    /// `(a, b)`, or the 1-tuple `(a,)`. Never empty.
    Tuple(Vec<Expr<'src>>),
    /// `[1, 2, 3]`, `[]`
    List(Vec<Expr<'src>>),
    /// `["a": 1, "b": 2]`, `[:]`
    Map(Vec<MapEntry<'src>>),

    // Block-like
    /// `{ stmt; stmt; tail }`. Without a tail, the block's value is `void`.
    Block {
        stmts: Vec<Stmt<'src>>,
        tail: Option<Box<Expr<'src>>>,
    },
    /// `if cond { … } else if other { … } else { … }`
    If {
        condition: Box<Expr<'src>>,
        /// Always a `Block`.
        then_branch: Box<Expr<'src>>,
        /// A `Block`, or an `If` for `else if`.
        else_branch: Option<Box<Expr<'src>>>,
    },
    /// `match shape { .Circle(r) => r * r, other => 0 }`
    Match {
        scrutinee: Box<Expr<'src>>,
        arms: Vec<MatchArm<'src>>,
    },
    /// `|x: number, y|: number x + y`
    Lambda {
        params: Delimited<LambdaParam<'src>>,
        /// `None` when omitted: inferred from the body or the expected type.
        ret: Option<Box<TypeAst<'src>>>,
        body: Box<Expr<'src>>,
    },
    /// `return value`, or a bare `return`.
    Return(Option<Box<Expr<'src>>>),

    // Operators
    /// `-x`, `!done`
    Unary {
        op: Spanned<UnaryOp>,
        operand: Box<Expr<'src>>,
    },
    /// `try parse(s)`: unwraps an `Ok`, or returns the `Err` from the enclosing function.
    Try {
        keyword: Span,
        operand: Box<Expr<'src>>,
    },
    /// `a + b`, `a ** b`, `a < b`, `a == b`
    Binary {
        left: Box<Expr<'src>>,
        op: Spanned<BinaryOp>,
        right: Box<Expr<'src>>,
    },
    /// `a and b`, `a or b`: the right side is evaluated only if needed.
    Logical {
        left: Box<Expr<'src>>,
        op: Spanned<LogicalOp>,
        right: Box<Expr<'src>>,
    },
    /// `maybe ?? fallback`, with the span of `??`.
    Coalesce {
        left: Box<Expr<'src>>,
        op: Span,
        right: Box<Expr<'src>>,
    },
    /// `shape is Circle`
    Is {
        operand: Box<Expr<'src>>,
        variant: Ident<'src>,
    },
    /// `maybe!`, with the span of `!`.
    ForceUnwrap {
        operand: Box<Expr<'src>>,
        bang: Span,
    },

    // Postfix access
    /// `f(1, label: 2)`, or `f?(x)` when `safe`.
    Call {
        callee: Box<Expr<'src>>,
        /// The `?(` when the call is safe.
        safe: Option<Span>,
        args: Delimited<CallArg<'src>>,
    },
    /// `point.x`, `pair.0`, or `maybe?.x` when `safe`.
    Field {
        object: Box<Expr<'src>>,
        /// The `?.` when the access is safe.
        safe: Option<Span>,
        member: Member<'src>,
    },
    /// `list[i]`, or `maybe?[i]` when `safe`.
    Index {
        object: Box<Expr<'src>>,
        /// The `?` when the access is safe.
        safe: Option<Span>,
        index: Box<Expr<'src>>,
    },
    /// `List.<number>`, `make.<T>`
    TypeSpecialization {
        callee: Box<Expr<'src>>,
        type_args: Delimited<TypeAst<'src>>,
    },

    // Assignment. `x += 1` is parsed as `x = x + 1`, with the `+=` span on the `+`.
    /// `x = value`
    Assign {
        target: Ident<'src>,
        value: Box<Expr<'src>>,
    },
    /// `point.x = value`, `maybe?.x = value`
    SetField {
        object: Box<Expr<'src>>,
        safe: Option<Span>,
        member: Member<'src>,
        value: Box<Expr<'src>>,
    },
    /// `list[i] = value`, `maybe?[i] = value`
    SetIndex {
        object: Box<Expr<'src>>,
        safe: Option<Span>,
        index: Box<Expr<'src>>,
        value: Box<Expr<'src>>,
    },
}

/// What a `.` selects.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Member<'src> {
    /// `point.x`, `list.push`: a field, method or variant name.
    Named(Ident<'src>),
    /// `pair.0`: a tuple element, as decimal digits without a leading zero.
    Index(Spanned<usize>),
}

impl Member<'_> {
    pub fn span(&self) -> Span {
        match self {
            Member::Named(name) => name.span,
            Member::Index(index) => index.span,
        }
    }
}

impl std::fmt::Display for Member<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Member::Named(name) => f.write_str(name.name),
            Member::Index(index) => write!(f, "{}", index.node),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    /// Never produced by the parser. The typechecker uses it for the value of a block without a tail and of a
    /// bare `return`.
    Void,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StringPart<'src> {
    /// The text between interpolations, with escapes processed. `span` covers its source text.
    Literal { value: String, span: Span },
    /// `${expr}`
    Expr(Expr<'src>),
}

/// `"key": value` in a map literal.
#[derive(Clone, Debug, PartialEq)]
pub struct MapEntry<'src> {
    pub key: Expr<'src>,
    pub value: Expr<'src>,
}

impl MapEntry<'_> {
    pub fn span(&self) -> Span {
        self.key.span.merge(self.value.span)
    }
}

/// `value` or `label: value` in a call.
#[derive(Clone, Debug, PartialEq)]
pub struct CallArg<'src> {
    pub label: Option<Ident<'src>>,
    pub value: Expr<'src>,
}

impl CallArg<'_> {
    pub fn span(&self) -> Span {
        self.label
            .map_or(self.value.span, |label| label.span.merge(self.value.span))
    }
}

/// `x` or `x: number` in a lambda.
#[derive(Clone, Debug, PartialEq)]
pub struct LambdaParam<'src> {
    pub name: Ident<'src>,
    /// `None` when omitted: inferred from the expected type.
    pub ty: Option<TypeAst<'src>>,
}

impl LambdaParam<'_> {
    pub fn span(&self) -> Span {
        self.ty
            .as_ref()
            .map_or(self.name.span, |ty| self.name.span.merge(ty.span))
    }
}

/// `pattern => body`
#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm<'src> {
    pub pattern: Pattern<'src>,
    pub body: Expr<'src>,
    /// From the pattern through the body, without the `,`.
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `!`
    Not,
    /// `-`
    Negate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}

impl UnaryOp {
    pub fn as_str(self) -> &'static str {
        match self {
            UnaryOp::Not => "!",
            UnaryOp::Negate => "-",
        }
    }
}

impl BinaryOp {
    pub fn as_str(self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Subtract => "-",
            BinaryOp::Multiply => "*",
            BinaryOp::Divide => "/",
            BinaryOp::Modulo => "%",
            BinaryOp::Power => "**",
            BinaryOp::Equal => "==",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
        }
    }
}

impl LogicalOp {
    pub fn as_str(self) -> &'static str {
        match self {
            LogicalOp::And => "and",
            LogicalOp::Or => "or",
        }
    }
}

// ---------------------------------------------------------------------------------------------------------------
// Patterns and bindings
// ---------------------------------------------------------------------------------------------------------------

/// The left side of a match arm.
#[derive(Clone, Debug, PartialEq)]
pub struct Pattern<'src> {
    pub kind: PatternKind<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PatternKind<'src> {
    /// `_`: matches anything, binds nothing.
    Wildcard,
    /// `other`: matches anything and binds it.
    Name(Ident<'src>),
    /// `.Circle(r)`, `Shape.Rect(w: width, :h)`, `.Empty`
    Variant {
        enum_name: Option<Ident<'src>>,
        variant: Ident<'src>,
        /// `None` without parentheses. Several payload bindings form a `Tuple` binding, and named ones a `Struct`
        /// binding named after the variant.
        payload: Option<Binding<'src>>,
    },
}

/// The left side of a `let`, or a match payload: what a value is destructured into.
#[derive(Clone, Debug, PartialEq)]
pub struct Binding<'src> {
    pub kind: BindingKind<'src>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BindingKind<'src> {
    /// `_`: ignores the value.
    Wildcard,
    /// `x`
    Name(Ident<'src>),
    /// `(a, b)`, `(a,)`. Never empty.
    Tuple(Vec<Binding<'src>>),
    /// `Point(x: px, :y)`
    Struct {
        name: Ident<'src>,
        fields: Vec<FieldBinding<'src>>,
    },
}

/// `field: binding`, or the shorthand `:name` for `name: name`.
#[derive(Clone, Debug, PartialEq)]
pub struct FieldBinding<'src> {
    pub field: Ident<'src>,
    pub binding: Binding<'src>,
    pub span: Span,
}
