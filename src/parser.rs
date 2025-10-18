use crate::ast::{Expr, Literal, Stmt};
use crate::scanner::Scanner;
use crate::token::{Token, TokenType};
use TokenType as TokT;
use std::fmt::{Display, Formatter};

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
            ParserError::UnexpectedToken {
                expected,
                found,
                message,
            } => {
                write!(
                    f,
                    "[line {}] Error at {}. Expected {:?} but found {:?} instead. {}",
                    found.line, found.lexeme, expected, found.token_type, message
                )
            }
            ParserError::MissingToken {
                expected,
                message,
                after_token,
            } => {
                write!(
                    f,
                    "[line {}] Error at {}. Missing {:?} but found {:?} instead. {}",
                    after_token.line, after_token.lexeme, expected, after_token.token_type, message
                )
            }
            ParserError::ParseError { token, message } => {
                write!(
                    f,
                    "[line {}] Error at {}. {}",
                    token.line, token.lexeme, message
                )
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
        if self.current_token.token_type == TokenType::Error {
            Err(self.error_current("Unexpected token."))
        } else {
            Ok(())
        }
    }

    fn consume(
        &mut self,
        token_type: TokenType,
        message: &'static str,
    ) -> Result<(), ParserError<'src>> {
        if self.current_token.token_type == token_type {
            self.advance()
        } else {
            Err(self.error_current(message))
        }
    }

    fn error_current(&mut self, message: &'static str) -> ParserError<'src> {
        self.panic_mode = true;
        self.had_error = true;
        ParserError::ParseError {
            token: self.current_token.clone(),
            message,
        }
    }
    fn error_previous(&mut self, message: &'static str) -> ParserError<'src> {
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
        } else {
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
        let expr = self.expression()?;
        self.consume(TokT::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(expr))
        // TODO Declarations and statements")
    }

    fn expression(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let expr = self.equality()?;

        if match_token_type!(self, TokenType::Equal) {
            let value = self.assignment()?;
            if let Expr::Identifier(name) = expr {
                return Ok(Expr::Assignment {
                    identifier: name,
                    value: Box::new(value),
                });
            }
            return Err(self.error_current("Invalid assignment target."));
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let mut expr = self.comparison()?;
        while match_token_type!(self, TokT::EqualEqual, TokT::BangEqual) {
            let op = self.previous_token.clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let mut expr = self.term()?;
        while match_token_type!(
            self,
            TokT::Greater,
            TokT::GreaterEqual,
            TokT::Less,
            TokT::LessEqual
        ) {
            let op = self.previous_token.clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let mut expr = self.factor()?;
        while match_token_type!(self, TokT::Minus, TokT::Plus) {
            let op = self.previous_token.clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let mut expr = self.unary()?;
        while match_token_type!(self, TokT::Star, TokT::Slash) {
            let op = self.previous_token.clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }
    fn unary(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::Bang, TokT::Minus) {
            let op = self.previous_token.clone();
            let expr = self.unary()?;
            Ok(Expr::Unary {
                operator: op,
                expression: Box::new(expr),
            })
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let expr = self.primary()?;
        if match_token_type!(self, TokT::LeftParen) {
            todo!()
        }
        Ok(expr)
    }
    fn primary(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        self.advance()?;
        match self.previous_token.token_type {
            TokT::True => self.literal(Literal::Boolean(true)),

            TokT::False => self.literal(Literal::Boolean(false)),
            TokT::Number => {
                if let Ok(num) = self.previous_token.lexeme.parse() {
                    self.literal(Literal::Number(num))
                } else {
                    Err(self.error_current("Expected number."))
                }
            }
            TokT::String => self.literal(Literal::String(String::from(
                &self.previous_token.lexeme[1..self.previous_token.lexeme.len() - 1],
            ))), // TODO Add string parsing
            TokT::Identifier => Ok(Expr::Identifier(self.previous_token.clone())),
            TokT::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokT::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::Grouping {
                    expression: Box::new(expr),
                })
            }
            _ => Err(self.error_current("Expect expression.")),
        }
    }
    fn literal(&mut self, literal: Literal) -> Result<Expr<'src>, ParserError<'src>> {
        Ok(Expr::Literal {
            literal,
            source: self.previous_token.clone(),
        })
    }
}
#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Literal, Stmt};
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::token::{Token, TokenType};

    #[test]
    fn test_basic_expressions() {
        let source = "4 -5;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse().expect("Failed to parse.").pop().unwrap();
        let expected = Expr::Binary {
            operator: Token::new(TokenType::Minus, 1, "-"),
            left: Box::from(Expr::Literal {
                literal: Literal::Number(4.0),
                source: Token::new(TokenType::Number, 1, "4"),
            }),
            right: Box::from(Expr::Literal {
                literal: Literal::Number(5.0),
                source: Token::new(TokenType::Number, 1, "5"),
            }),
        };
        println!("{:?}", res);
        let expected = Stmt::Expression(expected);

        assert_eq!(res, expected);
    }
    #[test]
    fn test_string_literals() {
        let source = r#" "hello world" == "hello world";"#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse().expect("Failed to parse.").pop().unwrap();
        println!("{:?}", res);

        let expected = Stmt::Expression(Expr::Binary {
            operator: Token::new(TokenType::EqualEqual, 1, "=="),
            left: Box::from(Expr::Literal {
                literal: Literal::String(String::from("hello world")),
                source: Token::new(TokenType::String, 1, r#""hello world""#),
            }),
            right: Box::from(Expr::Literal {
                literal: Literal::String(String::from("hello world")),
                source: Token::new(TokenType::String, 1, r#""hello world""#),
            }),
        });

        assert_eq!(res, expected);
    }
}
