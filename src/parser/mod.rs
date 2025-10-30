pub mod ast;
pub mod error;
mod expressions;
mod statements;

use crate::parser::error::ParserError;
use crate::scanner::Scanner;
use crate::token::TokenType as TokT;
use crate::token::{Token, TokenType};
use ast::Stmt;

pub struct Parser<'src> {
    scanner: Scanner<'src>,
    previous_token: Token<'src>,
    current_token: Token<'src>,
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

impl<'src> Parser<'src> {
    pub fn new(mut scanner: Scanner<'src>) -> Self {
        let start_token = scanner.next_token();
        Parser {
            scanner,
            previous_token: start_token.clone(),
            current_token: start_token,
        }
    }
    fn advance(&mut self) -> Result<(), ParserError<'src>> {
        self.previous_token = self.current_token.clone();
        self.current_token = self.scanner.next_token();
        if self.current_token.token_type == TokenType::UnexpectedSymbolError {
            Err(ParserError::UnexpectedToken {
                found: self.current_token.clone(),
                message: "Invalid token.",
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
        message: &'static str,
    ) -> Result<(), ParserError<'src>> {
        if self.current_token.token_type == token_type {
            self.advance()
        } else {
            Err(ParserError::MissingToken {
                expected: token_type,
                message,
                after_token: self.current_token.clone(),
            })
        }
    }

    fn error_current(&mut self, message: &'static str) -> ParserError<'src> {
        ParserError::ParseError {
            token: self.current_token.clone(),
            message,
        }
    }
    fn error_previous(&mut self, message: &'static str) -> ParserError<'src> {
        ParserError::ParseError {
            token: self.current_token.clone(),
            message,
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Stmt<'src>>, Vec<ParserError<'src>>> {
        let mut statements = vec![];
        let mut errors = vec![];
        while !self.scanner.is_at_end() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    errors.push(e);
                    self.synchronize();
                }
            }
        }
        if !errors.is_empty() {
            Err(errors.clone())
        } else {
            Ok(statements)
        }
    }

    fn synchronize(&mut self) {
        while !self.scanner.is_at_end() {
            if self.previous_token.token_type == TokenType::Semicolon {
                return;
            }
            if check_token_type!(
                self,
                TokT::Func,
                TokT::Let,
                TokT::If,
                TokT::While,
                TokT::Return
            ) {
                return;
            }
            let _ = self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expr, Literal, Stmt, Type};
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::token::{Token, TokenType};

    fn string(str: &str, line: usize) -> Box<Expr> {
        Box::new(Expr::Literal {
            literal: Literal::String(String::from(str)),
            line,
        })
    }
    fn number(num: f64, line: usize) -> Box<Expr<'static>> {
        Box::new(Expr::Literal {
            literal: Literal::Number(num),
            line,
        })
    }
    fn boolean(b: bool, line: usize) -> Box<Expr<'static>> {
        Box::new(Expr::Literal {
            literal: Literal::Boolean(b),
            line,
        })
    }

    fn var(name: &'static str, line: usize) -> Box<Expr<'static>> {
        Box::new(Expr::Variable {
            name: Token::new(TokenType::Identifier, line, name),
            scope: None,
            index: None,
        })
    }

    #[test]
    fn test_basic_expressions() {
        let source = "4 - 5;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse().expect("Failed to parse.").pop().unwrap();
        let expected = Expr::Binary {
            operator: Token::new(TokenType::Minus, 1, "-"),
            left: number(4.0, 1),
            right: number(5.0, 1),
        };
        let expected = Stmt::Expression(expected);

        assert_eq!(res, expected);
    }
    #[test]
    fn test_string_literals() {
        let source = r#" "hello world"
        == "hello world";"#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse().expect("Failed to parse.").pop().unwrap();

        let expected = Stmt::Expression(Expr::Binary {
            operator: Token::new(TokenType::EqualEqual, 2, "=="),
            left: string("hello world", 1),
            right: string("hello world", 2),
        });

        assert_eq!(res, expected);
    }
    #[test]
    fn test_if_statement() {
        let source = r#"
        func main(a: number, b: string):void {
            return;
        }

        let a = 10;

        while (true) {
            let b = a;
            return b;
        }

        {
            let a = 10;
            {
                let a = 20;
                println(a);
            }
            println(a);
        }
        "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse().expect("Failed to parse.");
        assert_eq!(res.len(), 4);
    }

    #[test]
    fn test_logical_operators() {
        let source = r#"
        let a:number = 10;
        while a > 0 and a < 20 {
            a=a+1;
        }
        "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse().expect("Failed to parse.");

        let mut expected = vec![];
        expected.push(Stmt::Let {
            identifier: Token {
                token_type: TokenType::Identifier,
                line: 2,
                lexeme: "a",
            },
            value: *number(10.0, 2),
            type_info: Type::Number,
        });
        expected.push(Stmt::While {
            condition: Expr::Logical {
                left: Box::new(Expr::Binary {
                    operator: Token {
                        token_type: TokenType::Greater,
                        line: 3,
                        lexeme: ">",
                    },
                    left: var("a", 3),
                    right: number(0.0, 3),
                }),
                operator: Token {
                    token_type: TokenType::And,
                    line: 3,
                    lexeme: "and",
                },
                right: Box::new(Expr::Binary {
                    operator: Token {
                        token_type: TokenType::Less,
                        line: 3,
                        lexeme: "<",
                    },
                    left: var("a", 3),
                    right: number(20.0, 3),
                }),
            },
            body: Box::new(Stmt::Block(vec![Stmt::Expression(Expr::Assignment {
                identifier: Token::new(TokenType::Identifier, 4, "a"),
                value: Box::new(Expr::Binary {
                    operator: Token {
                        token_type: TokenType::Plus,
                        line: 4,
                        lexeme: "+",
                    },
                    left: var("a", 4),
                    right: number(1.0, 4),
                }),
            })])),
        });
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parser_error_missing_semicolon() {
        let source = "let a = 10";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse();
        assert!(res.is_err(), "Parser should error on missing semicolon");
    }

    #[test]
    fn test_parser_error_invalid_type_identifier() {
        let source = "let a: foo = 10;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        assert!(
            parser.parse().is_err(),
            "Parser should error on invalid type identifier"
        );
    }

    #[test]
    fn test_parser_error_missing_right_paren() {
        let source = "let a = (10 + 2;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        assert!(
            parser.parse().is_err(),
            "Parser should error on missing right parenthesis"
        );
    }
}
