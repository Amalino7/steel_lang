use crate::ast::Type::Unknown;
use crate::ast::{Expr, Literal, Stmt, Type};
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
                    "[line {}] Error at '{}'. Expected {:?} but found {:?} instead. {}",
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
                    "[line {}] Error at '{}'. Missing {:?} but found {:?} instead. {}",
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

    fn declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::Let) {
            self.let_declaration()
        } else if match_token_type!(self, TokT::Func) {
            self.func_declaration()
        } else {
            self.statement()
        }
    }
    fn let_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if !match_token_type!(self, TokT::Identifier) {
            Err(self.error_current("Expected variable name."))
        } else {
            let name = self.previous_token.clone();
            let type_info = if match_token_type!(self, TokT::Colon) {
                self.consume(
                    TokT::Identifier,
                    "Expected the name of the variable type. Syntax: 'name: type'.",
                )?;
                if let Some(t) = Type::from_identifier(self.previous_token.clone()) {
                    t
                } else {
                    return Err(self.error_previous("Expected valid type identifier."));
                }
            } else {
                Unknown // The type will be inferred by the compiler later on.
            };

            self.consume(TokT::Equal, "Expected '=' after variable name.")?;
            let expr = self.expression()?;
            self.consume(TokT::Semicolon, "Expected ';' after variable declaration.'")?;
            Ok(Stmt::Let {
                identifier: name,
                value: expr,
                type_info,
            })
        }
    }

    // TODO rethink how to refactor this function
    fn func_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if !match_token_type!(self, TokT::Identifier) {
            Err(self.error_current("Expected function name."))
        } else {
            let name = self.previous_token.clone();
            self.consume(TokT::LeftParen, "Expected '(' after function name.")?;
            let mut params = vec![];
            let mut param_types = vec![];

            if !check_token_type!(self, TokT::RightParen) {
                loop {
                    self.consume(
                        TokT::Identifier,
                        "Expected the name of the function parameter.",
                    )?;
                    params.push(self.previous_token.clone());
                    self.consume(
                        TokT::Colon,
                        "Expected ':' after parameter name. Syntax: 'name: type",
                    )?;
                    self.consume(
                        TokT::Identifier,
                        "Expected the type after the function parameter name. Syntax: 'name: type'.",
                    )?;

                    if let Some(t) = Type::from_identifier(self.previous_token.clone()) {
                        param_types.push(t);
                    } else {
                        self.error_previous(
                            "Expected valid type identifier. Example: 'number, string, void'.",
                        );
                    }

                    if !match_token_type!(self, TokT::Comma) {
                        break;
                    }
                }
            }

            self.consume(TokT::RightParen, "Expected ')' after parameters.")?;

            let return_type = if match_token_type!(self, TokT::Colon) {
                self.consume(
                    TokT::Identifier,
                    "Expected the name of the return type of the function.",
                )?;
                if let Some(t) = Type::from_identifier(self.previous_token.clone()) {
                    t
                } else {
                    return Err(self.error_previous("Expected valid type identifier"));
                }
            } else {
                Type::Void
            };

            self.consume(TokT::LeftBrace, "Expected '{' before function body.")?;
            let mut body = vec![];

            while !match_token_type!(self, TokT::RightBrace) {
                body.push(self.declaration()?);
            }

            Ok(Stmt::Function {
                type_: Type::Function {
                    param_types,
                    return_type: Box::new(return_type),
                },
                name,
                params,
                body,
            })
        }
    }
    fn statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if check_token_type!(self, TokT::LeftBrace) {
            println!("{:?}", self.current_token);
            self.block()
        } else if match_token_type!(self, TokT::If) {
            self.if_statement()
        } else if match_token_type!(self, TokT::While) {
            self.while_statement()
        } else if match_token_type!(self, TokT::Return) {
            let val = if !match_token_type!(self, TokT::Semicolon) {
                let expr = self.expression()?;
                self.consume(TokT::Semicolon, "Expect ';' after return value.")?;
                expr
            } else {
                Expr::Literal(Literal::Void)
            };
            Ok(Stmt::Return(val))
        } else {
            let expr = self.expression()?;
            self.consume(TokT::Semicolon, "Expect ';' after expression.")?;
            Ok(Stmt::Expression(expr))
        }
    }
    fn block(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::LeftBrace, "Expected '{' before block.")?;
        let mut statements = vec![];
        while !check_token_type!(self, TokT::RightBrace) {
            statements.push(self.declaration()?);
        }

        self.consume(TokT::RightBrace, "Expected '}' after block.")?;
        Ok(Stmt::Block(statements))
    }
    fn if_statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let condition = self.expression()?;
        let then_branch = self.block()?;
        let mut else_branch = None;
        if match_token_type!(self, TokT::Else) {
            else_branch = Some(self.block()?);
        }
        Ok(Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(|e| Box::new(e)),
        })
    }
    fn while_statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let condition = self.expression()?;
        let body = self.block()?;
        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }

    fn expression(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let mut left = self.logical_and()?;

        while match_token_type!(self, TokT::Or) {
            let operator = self.previous_token.clone();
            let right = self.logical_and()?;
            left = Expr::Logical {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        Ok(left)
    }
    fn logical_and(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let mut left = self.assignment()?;
        while match_token_type!(self, TokT::And) {
            let operator = self.previous_token.clone();
            let right = self.assignment()?;
            left = Expr::Logical {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn assignment(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let expr = self.equality()?;

        if match_token_type!(self, TokenType::Equal) {
            let value = self.assignment()?;
            if let Expr::Variable { name, .. } = expr {
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
        let mut expr = self.primary()?;
        if match_token_type!(self, TokT::LeftParen) {
            let args = self.arguments()?;
            expr = Expr::Call {
                callee: Box::new(expr),
                arguments: args,
            }
        }
        Ok(expr)
    }
    fn arguments(&mut self) -> Result<Vec<Expr<'src>>, ParserError<'src>> {
        let mut args = vec![];
        if !check_token_type!(self, TokT::RightParen) {
            args.push(self.expression()?);
        }
        while match_token_type!(self, TokT::Comma) {
            args.push(self.expression()?);
        }

        self.consume(
            TokT::RightParen,
            "Expected ')' after function call arguments.",
        )?;

        Ok(args)
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
            TokT::Identifier => Ok(Expr::Variable {
                name: self.previous_token.clone(),
                scope: None,
                index: None,
            }),
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
        Ok(Expr::Literal(literal))
    }
}
#[cfg(test)]
mod tests {
    use crate::ast::{Expr, Literal, Stmt};
    use crate::parser::Parser;
    use crate::scanner::Scanner;
    use crate::token::{Token, TokenType};

    fn string(str: &str) -> Box<Expr> {
        Box::new(Expr::Literal(Literal::String(String::from(str))))
    }
    fn number(num: f64) -> Box<Expr<'static>> {
        Box::new(Expr::Literal(Literal::Number(num)))
    }
    fn boolean(b: bool) -> Box<Expr<'static>> {
        Box::new(Expr::Literal(Literal::Boolean(b)))
    }

    #[test]
    fn test_basic_expressions() {
        let source = "4 - 5;";
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let res = parser.parse().expect("Failed to parse.").pop().unwrap();
        let expected = Expr::Binary {
            operator: Token::new(TokenType::Minus, 1, "-"),
            left: number(4.0),
            right: number(5.0),
        };
        println!("{:?}", res);
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
        println!("{:?}", res);

        let expected = Stmt::Expression(Expr::Binary {
            operator: Token::new(TokenType::EqualEqual, 2, "=="),
            left: string("hello world"),
            right: string("hello world"),
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
        println!("{:#?}", res);
        res.iter().for_each(|stmt| println!("{}", stmt));
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
        println!("{:#?}", res);
        res.iter().for_each(|stmt| println!("{}", stmt));
    }
}
