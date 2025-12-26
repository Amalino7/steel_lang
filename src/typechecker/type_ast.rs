use crate::compiler::analysis::ResolvedVar;
use crate::parser::ast::Literal;
use crate::typechecker::types::Type;
use crate::typechecker::Symbol;

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub ty: Type,
    pub kind: ExprKind,
    pub line: u32,
}
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
    Unwrap,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOp {
    Or,
    And,
    Coalesce,
}

#[derive(Debug, Clone, PartialEq)]
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
    GetVar(ResolvedVar, Symbol),
    GetField {
        object: Box<TypedExpr>,
        index: u8,
        safe: bool,
    },
    SetField {
        object: Box<TypedExpr>,
        index: u8,
        safe: bool,
        value: Box<TypedExpr>,
    },
    StructInit {
        name: Box<str>,
        args: Vec<TypedExpr>,
    },
    EnumInit {
        enum_name: Symbol,
        variant_idx: u16,
        value: Box<TypedExpr>,
    },
    InterfaceUpcast {
        expr: Box<TypedExpr>,
        vtable_idx: u32,
    },
    Is {
        target: Box<TypedExpr>,
        variant_idx: u16,
        narrowed: Type,
    },
    InterfaceMethodGet {
        object: Box<TypedExpr>,
        method_index: u8,
        safe: bool,
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
        typed_refinements: Vec<(ResolvedVar, ResolvedVar)>,
        right: Box<TypedExpr>,
    },
    Assign {
        target: ResolvedVar,
        value: Box<TypedExpr>,
    },
    MethodGet {
        object: Box<TypedExpr>,
        method: ResolvedVar,
        safe: bool,
    },
    Tuple {
        elements: Vec<TypedExpr>,
    },
    Call {
        callee: Box<TypedExpr>, // 8 bytes
        arguments: Vec<TypedExpr>,
        safe: bool,
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
    EnumDecl {},
    Global {
        global_count: u32,
        stmts: Vec<TypedStmt>,
        reserved: u16,
    },
    Expression(TypedExpr),
    Return(TypedExpr),
    Let {
        binding: TypedBinding,
        value: TypedExpr,
    },
    Block {
        body: Vec<TypedStmt>,
        reserved: u16,
    },
    If {
        condition: TypedExpr,
        then_branch: Box<TypedStmt>,
        else_branch: Option<Box<TypedStmt>>,
        typed_refinements: Box<TypedRefinements>,
    },
    Match {
        value: Box<TypedExpr>,
        cases: Vec<MatchCase>,
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
#[derive(Debug)]
pub enum TypedBinding {
    Variable(ResolvedVar),
    Ignored,
    Tuple(Vec<TypedBinding>),
    Struct(Vec<(u8, TypedBinding)>),
}

#[derive(Debug)]
pub enum MatchCase {
    Variable {
        binding: TypedBinding,
        body: TypedStmt,
    },
    Named {
        variant_name: String,
        variant_idx: u16,
        binding: TypedBinding,
        body: TypedStmt,
    },
}
#[derive(Debug)]
pub struct TypedRefinements {
    pub true_path: Vec<(ResolvedVar, ResolvedVar)>,
    pub else_path: Vec<(ResolvedVar, ResolvedVar)>,
    pub after_path: Vec<(ResolvedVar, ResolvedVar)>,
}
