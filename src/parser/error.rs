//! Parser and lexer errors, and their conversion into diagnostics.
//!
//! Every variant carries the span it is reported at. Expected-token errors compute that span once, in
//! `Parser::err_expected`, so the position rule lives in one place. Phase 3 of the parser plan replaces the
//! free-form messages with an error-kind enum.

use crate::diagnostics::builder::DiagBuilder;
use crate::diagnostics::{Diagnostic, DiagnosticContext, IntoDiagnostic};
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
    /// `span` is `found`'s span, or the point after the previous token when `found` starts a new line.
    MissingToken {
        expected: TokenType,
        /// Noun phrase for "expected X after <after>".
        after: &'static str,
        found: Token<'src>,
        span: Span,
    },
    /// A closing delimiter is missing; `open` is the opener it would close.
    Unclosed {
        close: TokenType,
        open: Span,
        found: Token<'src>,
        span: Span,
    },
    ParseError {
        token: Token<'src>,
        message: String,
    },
    /// A construct that parsed but isn't allowed, reported on the whole construct.
    Invalid {
        span: Span,
        message: String,
    },
    InvalidAssignmentTarget {
        span: Span,
    },
}
impl<'src> ParserError<'src> {
    pub fn span(&self) -> Span {
        match self {
            ParserError::ScannerError { token } => token.span,
            ParserError::UnexpectedToken { found, .. } => found.span,
            ParserError::ParseError { token, .. } => token.span,
            ParserError::MissingToken { span, .. } => *span,
            ParserError::Unclosed { span, .. } => *span,
            ParserError::Invalid { span, .. } => *span,
            ParserError::InvalidAssignmentTarget { span } => *span,
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
                expected: TokenType::Identifier,
                after,
                found,
                ..
            } => format!("Expected a name after {after}. Found '{}'.", found.lexeme),
            ParserError::MissingToken {
                expected,
                after,
                found,
                ..
            } => format!(
                "Missing '{expected}' after {after}. Found '{}'.",
                found.lexeme
            ),
            ParserError::Unclosed { close, found, .. } => {
                format!(
                    "Missing '{close}' to close the delimiter. Found '{}'.",
                    found.lexeme
                )
            }
            ParserError::ParseError { message, .. } => message.to_string(),
            ParserError::Invalid { message, .. } => message.to_string(),
            ParserError::InvalidAssignmentTarget { .. } => "Invalid assignment target.".to_string(),
        }
    }
}

impl Display for ParserError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl IntoDiagnostic for ParserError<'_> {
    fn into_diagnostic(self, _: &DiagnosticContext) -> Diagnostic {
        let mut builder = DiagBuilder::error(self.span(), "E0000", "Parser error", self.message());
        match &self {
            ParserError::MissingToken { found, span, .. } if found.span != *span => {
                builder = builder.with_secondary(found.span, "unexpected token");
            }
            ParserError::Unclosed {
                open, found, span, ..
            } => {
                builder = builder.with_secondary(*open, "unclosed delimiter");
                if found.span != *span {
                    builder = builder.with_secondary(found.span, "unexpected token");
                }
            }
            _ => {}
        }
        builder.build()
    }
}
