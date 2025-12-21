use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src> {
    pub token_type: TokenType,
    pub line: u32,
    pub(crate) lexeme: &'src str,
}

impl<'src> Token<'src> {
    pub fn new(token_type: TokenType, line: u32, lexeme: &'src str) -> Self {
        Self {
            token_type,
            line,
            lexeme,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenType {
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,
    Dot,
    Semicolon,
    Colon,
    Minus,
    Plus,
    Slash,
    Star,
    Question,         // ?
    QuestionDot,      // ?.
    QuestionParen,    // ?(
    QuestionQuestion, // ??

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Arrow, // =>

    // Assignment arithmetic operators.
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Or,
    If,
    Else,
    Func,
    Return,
    True,
    Nil,
    False,
    Let,
    While,
    Struct,
    Impl,
    Self_,
    Interface,

    Enum,
    Match,

    // Reports errors.
    Error,
    UnexpectedSymbolError,

    EOF,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => f.write_str("("),
            TokenType::RightParen => f.write_str(")"),
            TokenType::LeftBrace => f.write_str("{"),
            TokenType::RightBrace => f.write_str("}"),
            TokenType::LeftBracket => f.write_str("["),
            TokenType::RightBracket => f.write_str("]"),
            TokenType::Comma => f.write_str(","),
            TokenType::Dot => f.write_str("."),
            TokenType::Semicolon => f.write_str(";"),
            TokenType::Colon => f.write_str(":"),
            TokenType::Minus => f.write_str("-"),
            TokenType::Plus => f.write_str("+"),
            TokenType::Slash => f.write_str("/"),
            TokenType::Star => f.write_str("*"),
            TokenType::Bang => f.write_str("!"),
            TokenType::BangEqual => f.write_str("!="),
            TokenType::Equal => f.write_str("="),
            TokenType::EqualEqual => f.write_str("=="),
            TokenType::Greater => f.write_str(">"),
            TokenType::GreaterEqual => f.write_str(">="),
            TokenType::Less => f.write_str("<"),
            TokenType::LessEqual => f.write_str("<="),
            TokenType::Identifier => f.write_str("identifier"),
            TokenType::String => f.write_str("string"),
            TokenType::Number => f.write_str("number"),
            TokenType::And => f.write_str("and"),
            TokenType::Or => f.write_str("or"),
            TokenType::If => f.write_str("if"),
            TokenType::Else => f.write_str("else"),
            TokenType::Func => f.write_str("func"),
            TokenType::Return => f.write_str("return"),
            TokenType::True => f.write_str("true"),
            TokenType::False => f.write_str("false"),
            TokenType::Let => f.write_str("let"),
            TokenType::While => f.write_str("while"),
            TokenType::Error => f.write_str("error"),
            TokenType::UnexpectedSymbolError => f.write_str("unexpected symbol"),
            TokenType::PlusEqual => f.write_str("+="),
            TokenType::MinusEqual => f.write_str("-="),
            TokenType::StarEqual => f.write_str("*="),
            TokenType::SlashEqual => f.write_str("/="),
            TokenType::EOF => f.write_str("nothing"),
            TokenType::Struct => f.write_str("struct"),
            TokenType::Impl => f.write_str("impl"),
            TokenType::Self_ => f.write_str("self"),
            TokenType::Interface => f.write_str("interface"),
            TokenType::Question => f.write_str("?"),
            TokenType::QuestionDot => f.write_str("?."),
            TokenType::QuestionQuestion => f.write_str("??"),
            TokenType::QuestionParen => f.write_str("?("),
            TokenType::Nil => f.write_str("nil"),
            TokenType::Enum => f.write_str("enum"),
            TokenType::Match => f.write_str("match"),
            TokenType::Arrow => f.write_str("=>"),
        }
    }
}
