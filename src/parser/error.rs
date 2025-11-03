use crate::token::{Token, TokenType};
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

impl Display for ParserError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken { found, message } => {
                write!(
                    f,
                    "[line {}] Error at '{}'.\n{}\n",
                    found.line, found.lexeme, message
                )
            }
            ParserError::MissingToken {
                expected,
                message,
                after_token,
            } => {
                write!(
                    f,
                    "[line {}] Error at '{}'. Missing '{}' but found '{}' instead.\nNote: {}",
                    after_token.line, after_token.lexeme, expected, after_token.token_type, message
                )
            }
            ParserError::ParseError { token, message } => {
                write!(
                    f,
                    "[line {}] Error at '{}'. {}",
                    token.line, token.lexeme, message
                )
            }
            ParserError::ScannerError { token } => {
                write!(f, "[line {}] Error: {}", token.line, token.lexeme)
            }
        }
    }
}
