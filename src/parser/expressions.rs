use crate::parser::ast::Expr::Tuple;
use crate::parser::ast::{CallArg, Expr, Literal};
use crate::parser::error::ParserError;
use crate::parser::TokT;
use crate::parser::{check_next_token_type, check_token_type};
use crate::parser::{match_token_type, Parser};
use crate::token::Token;

impl<'src> Parser<'src> {
    pub(crate) fn expression(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        self.null_coalescing()
    }

    fn null_coalescing(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let mut expr = self.logical_or()?;
        while match_token_type!(self, TokT::QuestionQuestion) {
            let op = self.previous_token.clone();
            let right = self.logical_or()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: op,
                right: Box::new(right),
            };
        }
        Ok(expr)
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

    fn handle_assigment(
        &mut self,
        expr: Expr<'src>,
        value: Expr<'src>,
    ) -> Result<Expr<'src>, ParserError<'src>> {
        if let Expr::Variable { name, .. } = expr {
            Ok(Expr::Assignment {
                identifier: name,
                value: Box::new(value),
            })
        } else if let Expr::Get {
            object,
            field,
            safe,
        } = expr
        {
            Ok(Expr::Set {
                safe,
                object,
                field,
                value: Box::new(value),
            })
        } else {
            Err(self.error_current("Invalid assignment target."))
        }
    }

    fn assignment(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let expr = self.equality()?;

        if match_token_type!(self, TokT::Equal) {
            let value = self.assignment()?;
            return self.handle_assigment(expr, value);
        } else if match_token_type!(
            self,
            TokT::PlusEqual,
            TokT::MinusEqual,
            TokT::StarEqual,
            TokT::SlashEqual
        ) {
            let op = self.previous_token.clone();
            let left = expr.clone();
            let right = self.assignment()?;

            let binary = match op.token_type {
                TokT::PlusEqual => Expr::Binary {
                    left: Box::new(left),
                    operator: Token::new(TokT::Plus, op.line, op.lexeme),
                    right: Box::new(right),
                },
                TokT::MinusEqual => Expr::Binary {
                    left: Box::new(left),
                    operator: Token::new(TokT::Minus, op.line, op.lexeme),
                    right: Box::new(right),
                },
                TokT::StarEqual => Expr::Binary {
                    left: Box::new(left),
                    operator: Token::new(TokT::Star, op.line, op.lexeme),
                    right: Box::new(right),
                },
                TokT::SlashEqual => Expr::Binary {
                    left: Box::new(left),
                    operator: Token::new(TokT::Slash, op.line, op.lexeme),
                    right: Box::new(right),
                },
                _ => unreachable!(),
            };

            let target = self.handle_assigment(expr, binary)?;
            return Ok(target);
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
        loop {
            if match_token_type!(self, TokT::LeftParen, TokT::QuestionParen) {
                let prev = self.previous_token.clone();
                let args = self.arguments()?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    arguments: args,
                    safe: prev.token_type == TokT::QuestionParen,
                };
            } else if match_token_type!(self, TokT::Dot) {
                if match_token_type!(self, TokT::Number) {
                    let idx = self.previous_token.clone();
                    expr = Expr::Get {
                        safe: false,
                        object: Box::new(expr),
                        field: idx,
                    };
                    continue;
                }

                self.consume(TokT::Identifier, "Expected property name after '.'.")?;
                let field = self.previous_token.clone();
                expr = Expr::Get {
                    safe: false,
                    object: Box::new(expr),
                    field,
                };
            } else if match_token_type!(self, TokT::QuestionDot) {
                self.consume(TokT::Identifier, "Expected property name after '?.'.")?;
                let field = self.previous_token.clone();
                expr = Expr::Get {
                    safe: true,
                    object: Box::new(expr),
                    field,
                };
            } else if match_token_type!(self, TokT::Bang) {
                expr = Expr::ForceUnwrap {
                    expression: Box::new(expr),
                    line: self.previous_token.line,
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn arguments(&mut self) -> Result<Vec<CallArg<'src>>, ParserError<'src>> {
        let mut args = vec![];
        if !check_token_type!(self, TokT::RightParen) {
            args.push(self.parse_call_arg()?);
        }
        while match_token_type!(self, TokT::Comma) {
            args.push(self.parse_call_arg()?);
        }

        self.consume(
            TokT::RightParen,
            "Expected ')' after function call arguments.",
        )?;

        Ok(args)
    }

    fn parse_call_arg(&mut self) -> Result<CallArg<'src>, ParserError<'src>> {
        if check_token_type!(self, TokT::Identifier) && check_next_token_type!(self, TokT::Colon) {
            self.consume(TokT::Identifier, "Expected label name.")?;
            let label = Some(self.previous_token.clone());
            self.consume(TokT::Colon, "Expected ':' after label.")?;
            let expr = self.expression()?;
            Ok(CallArg { label, expr })
        } else {
            let expr = self.expression()?;
            Ok(CallArg { label: None, expr })
        }
    }

    fn primary(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        self.advance()?;
        match self.previous_token.token_type {
            TokT::True => self.literal(Literal::Boolean(true)),
            TokT::Nil => self.literal(Literal::Nil),
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
            }),
            TokT::Self_ => Ok(Expr::Variable {
                name: self.previous_token.clone(),
            }),
            TokT::LeftParen => {
                let expr = self.expression()?;
                let mut tuple_args = vec![];
                if match_token_type!(self, TokT::Comma) {
                    tuple_args.push(expr);
                    while !match_token_type!(self, TokT::RightParen) {
                        let expr = self.expression()?;
                        tuple_args.push(expr);
                    }
                    return Ok(Tuple {
                        elements: tuple_args,
                    });
                }

                self.consume(TokT::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::Grouping {
                    expression: Box::new(expr),
                })
            }
            _ => Err(self.error_previous("Expect expression.")),
        }
    }
    fn literal(&mut self, literal: Literal) -> Result<Expr<'src>, ParserError<'src>> {
        Ok(Expr::Literal {
            literal,
            line: self.previous_token.line,
        })
    }
}
