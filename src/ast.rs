use crate::token::Token;

#[derive(Debug, PartialEq,Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Number,
    String,
    Boolean,
    Void,
    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
    Unknown,
    Error,
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
    Identifier(Token<'src>),
    Grouping {
        expression: Box<Expr<'src>>,
    },
    Literal{
      literal: Literal,
        source: Token<'src>,
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
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'src> {
    Expression(Expr<'src>),
    Let {
        identifier: Token<'src>,
        value: Expr<'src>,
    },
    Block(Vec<Stmt<'src>>),
    If {
        condition: Expr<'src>,
        then_branch: Box<Stmt<'src>>,
        else_branch: Option<Box<Stmt<'src>>>,
    },
    While {
        condition: Expr<'src>,
        body: Box<Stmt<'src>>,
    },
    Function{
        name: Token<'src>,
        params: Vec<Token<'src>>,
        body: Vec<Stmt<'src>>,
    },
    Return(Expr<'src>),
}

