pub mod ast;
mod control_flow;
mod declarations;
pub mod error;
mod expressions;
mod literals;
pub mod printer;
mod statements;

use crate::parser::error::ParserError;
use crate::scanner::Scanner;
use crate::scanner::TokenType as TokT;
use crate::scanner::{Token, TokenType};
use ast::ParsedFile;
use std::mem;

pub struct Parser<'src> {
    scanner: Scanner<'src>,
    previous_token: Token<'src>,
    current_token: Token<'src>,
    next_token: Token<'src>,
    errors: Vec<ParserError<'src>>,
}
macro_rules! check_token_type {
    ($parser:expr, $( $token_type:pat $(,)?)*) => {
        match $parser.current_token.token_type {
            $( $token_type )|+  => true,
            _ => false,
        }
    };
}

use check_token_type;
macro_rules! match_token_type {
    ($parser:expr, $( $token_type:pat $(,)?)*) => {
        if check_token_type!($parser, $( $token_type ),+ ) {
            $parser.advance()?;
            true
        }
        else {
            false
        }
    };
}

use match_token_type;

macro_rules! check_next_token_type {
    ($parser:expr, $( $token_type:pat $(,)?)*) => {
        match $parser.next_token.token_type {
            $( $token_type )|+  => true,
            _ => false,
        }
    };
}
use check_next_token_type;

impl<'src> Parser<'src> {
    pub fn new(mut scanner: Scanner<'src>) -> Self {
        let start_token = scanner.next_token();
        let next = scanner.next_token();
        Parser {
            scanner,
            previous_token: start_token.clone(),
            current_token: start_token,
            next_token: next,
            errors: Vec::new(),
        }
    }
    fn advance(&mut self) -> Result<(), ParserError<'src>> {
        self.previous_token = self.current_token.clone();
        self.current_token = self.next_token.clone();
        self.next_token = self.scanner.next_token();
        if self.current_token.token_type == TokenType::UnexpectedSymbolError {
            Err(ParserError::UnexpectedToken {
                found: self.current_token.clone(),
                message: "Invalid token".to_string(),
            })
        } else if self.current_token.token_type == TokenType::Error {
            Err(ParserError::ScannerError {
                token: self.current_token.clone(),
            })
        } else {
            Ok(())
        }
    }

    fn consume(
        &mut self,
        token_type: TokenType,
        message: impl Into<String>,
    ) -> Result<(), ParserError<'src>> {
        if self.current_token.token_type == token_type {
            self.advance()
        } else {
            Err(ParserError::MissingToken {
                expected: token_type,
                message: message.into(),
                found_token: self.current_token.clone(),
            })
        }
    }

    fn error_current(&mut self, message: impl Into<String>) -> ParserError<'src> {
        ParserError::ParseError {
            token: self.current_token.clone(),
            message: message.into(),
        }
    }
    fn error_previous(&mut self, message: impl Into<String>) -> ParserError<'src> {
        ParserError::ParseError {
            token: self.previous_token.clone(),
            message: message.into(),
        }
    }
    fn at_eof(&self) -> bool {
        self.current_token.token_type == TokT::EOF
    }

    pub fn parse(&mut self) -> Result<ParsedFile<'src>, Vec<ParserError<'src>>> {
        let imports = self.parse_imports();
        let mut statements = vec![];
        while !self.at_eof() {
            match self.declaration(true) {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.errors.push(e);
                    self.synchronize_top_level();
                }
            }
        }
        if !self.errors.is_empty() {
            Err(mem::take(&mut self.errors))
        } else {
            Ok(ParsedFile {
                imports,
                body: statements,
            })
        }
    }

    /// Like [`Self::synchronize`], but a `}` is never a restart point at the top level, so it is skipped
    /// instead of being reported again as the start of the next statement.
    fn synchronize_top_level(&mut self) {
        // TODO refactor synchronization logic.
        self.synchronize();
        while check_token_type!(self, TokT::RightBrace) {
            let _ = self.advance();
            self.synchronize();
        }
    }

    fn synchronize(&mut self) {
        while !self.at_eof() {
            if self.previous_token.token_type == TokenType::Semicolon {
                return;
            }
            if check_token_type!(
                self,
                TokT::Func,
                TokT::RightBrace,
                TokT::Match,
                TokT::Let,
                TokT::If,
                TokT::While,
                TokT::Return,
                TokT::Enum,
                TokT::Struct,
                TokT::Impl,
                TokT::Import,
            ) {
                return;
            }
            let _ = self.advance();
        }
    }
}

#[cfg(test)]
mod tests;
