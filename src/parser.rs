use std::fmt::{Display, Formatter};
use crate::ast::{Expr, Stmt};
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use TokenType as TokT;
pub struct Parser<'src> {
    scanner: Scanner<'src>,
    previous_token: Token<'src>,
    current_token: Token<'src>,
    panic_mode: bool,
    had_error: bool,
}

macro_rules! check_token_type {
    ($parser:expr, $( $token_type:pat $(,)?)*) => {
        match $parser.current_token.token_type {
            $( $token_type )|+  => true,
            _ => false,
        }
    };
}
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

#[derive(Debug, Clone)]
pub enum ParserError<'src> {
    UnexpectedToken {
        expected: Option<TokenType>,
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
            ParserError::UnexpectedToken { expected, found, message } => {
                write!(f, "[line {}] Error at {}. Expected {:?} but found {:?} instead. {}"
                       , found.line, found.lexeme, expected, found.token_type, message)
            }
            ParserError::MissingToken {expected,message, after_token } => {
                write!(f, "[line {}] Error at {}. Missing {:?} but found {:?} instead. {}"
                       , after_token.line, after_token.lexeme, expected, after_token.token_type, message)
            }
            ParserError::ParseError {token, message  } => {
                write!(f, "[line {}] Error at {}. {}", token.line, token.lexeme, message)
            }
        }
    }
}


impl<'src> Parser<'src> {
    pub fn new(mut scanner: Scanner<'src>) -> Self {
        let start_token = scanner.next_token();
        Parser {
            scanner,
            previous_token: start_token.clone(),
            current_token: start_token,
            panic_mode: false,
            had_error: false,
        }
    }
    fn advance(&mut self) -> Result<(), ParserError<'src>> {
        self.previous_token = self.current_token.clone();
        self.current_token = self.scanner.next_token();


        self.current_token = self.scanner.next_token();
        if self.current_token.token_type == TokenType::Error {
            Err(self.error_current("Unexpected token."))
        }
        else {
          Ok(())
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &'static str) -> Result<(), ParserError<'src>> {
        if self.current_token.token_type == token_type {
            self.advance()
        } else {
            Err(self.error_current(message))
        }
    }

    fn error_current(&mut self, message: &'static str) ->  ParserError<'src>{
        self.panic_mode = true;
        self.had_error = true;
        ParserError::ParseError {
            token: self.current_token.clone(),
            message,
        }
    }
    fn error_previous(&mut self, message: &'static str) -> ParserError<'src>{
        self.panic_mode = true;
        self.had_error = true;
        ParserError::ParseError {
            token: self.previous_token.clone(),
            message,
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Stmt<'src>>, ()> {
        let mut statements = vec![];
        while !self.scanner.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    eprintln!("{}", e);
                    self.synchronize();
                }
            }
        }
        if self.had_error {
            Err(())
        }else {
            Ok(statements)
        }
    }
    fn synchronize(&mut self) {
        self.panic_mode = false;
        while !self.scanner.is_at_end() {
            if self.previous_token.token_type == TokT::Semicolon {
                return;
            }
            if check_token_type!(self, TokT::Func, TokT::Let, TokT::If, TokT::While) {
                return;
            }
            let _ = self.advance();
        }
    }
    fn declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        todo!("Add declarations")
    }
}

