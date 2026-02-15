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
        message: String,
    },
    MissingToken {
        expected: TokenType,
        found_token: Token<'src>,
        message: String,
    },
    ParseError {
        token: Token<'src>,
        message: String,
    },
}
impl<'src> ParserError<'src> {
    pub fn span(&self) -> Span {
        match self {
            ParserError::ScannerError { token } => token.span,
            ParserError::UnexpectedToken { found, .. } => found.span,
            ParserError::ParseError { token, .. } => token.span,
            ParserError::MissingToken { found_token, .. } => found_token.span,
        }
    }
    pub fn message(&self) -> String {
        match self {
            ParserError::ScannerError { token } => {
                format!("Scanner error: {}", token.lexeme)
            }
            ParserError::UnexpectedToken { found, message } => {
                format!("{} Found '{}'.", message, found.lexeme)
            }
            ParserError::MissingToken {
                expected,
                found_token,
                message,
            } => {
                format!(
                    "Missing '{}'. {} Found '{}'.",
                    expected, message, found_token.lexeme
                )
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
