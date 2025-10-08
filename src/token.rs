#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub struct Token<'src> {
    pub token_type: TokenType,
    pub line: usize,
    pub(crate) lexeme: &'src str,
}

impl<'src> Token<'src> {
    pub fn new(token_type: TokenType, line: usize, lexeme: &'src str) -> Self {
        Self {
            token_type,
            line,
            lexeme,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TokenType {
    LeftParen, // (
    RightParen, // )
    LeftBrace, // {
    RightBrace, // }
    LeftBracket, // [
    RightBracket, // ]
    Comma,
    Dot,
    Semicolon,
    Minus,
    Plus,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

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
    False,
    Let,
    While,

    // Reports errors.
    Error,

    EOF,
}
