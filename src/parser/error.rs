use crate::scanner::Span;
use crate::scanner::{Token, TokenType};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum ParserError<'src> {
    ScannerError {
        token: Token<'src>,
    },
    UnexpectedToken {
        found: Token<'src>,
        message: &'static str,
    },
    MissingToken {
        expected: TokenType,
        after_token: Token<'src>,
        message: &'static str,
    },
    ParseError {
        token: Token<'src>,
        message: &'static str,
    },
}
impl<'src> ParserError<'src> {
    pub fn span(&self) -> Span {
        match self {
            ParserError::ScannerError { token } => token.span,
            ParserError::UnexpectedToken { found, .. } => found.span,
            ParserError::ParseError { token, .. } => token.span,
            ParserError::MissingToken { after_token, .. } => after_token.span,
        }
    }
    pub fn message(&self) -> String {
        match self {
            ParserError::ScannerError { token } => {
                format!("Scanner error: {}", token.lexeme)
            }
            ParserError::UnexpectedToken { message, .. } => message.to_string(),
            ParserError::MissingToken {
                expected, message, ..
            } => {
                format!("Missing '{}'. {}", expected, message)
            }
            ParserError::ParseError { message, .. } => message.to_string(),
        }
    }
}

impl Display for ParserError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}
