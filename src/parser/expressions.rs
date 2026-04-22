use crate::parser::ast::Expr::Tuple;
use crate::parser::ast::{CallArg, Expr, ExprMatchArm, Literal, Stmt, StringPart, TypeAst};
use crate::parser::error::ParserError;
use crate::parser::literals::{parse_number, process_escapes, process_raw_string};
use crate::parser::TokT;
use crate::parser::{check_next_token_type, check_token_type};
use crate::parser::{match_token_type, Parser};
use crate::scanner::token::Token;

impl<'src> Parser<'src> {
    pub(crate) fn expression(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let expr = self.null_coalescing()?;

        if match_token_type!(self, TokT::Equal) {
            let value = self.assignment()?;
            return self.handle_assigment(expr, value);
        } else if match_token_type!(
            self,
            TokT::PlusEqual,
            TokT::MinusEqual,
            TokT::StarEqual,
            TokT::SlashEqual,
            TokT::PercentEqual
        ) {
            let op = self.previous_token.clone();
            let left = expr.clone();
            let right = self.assignment()?;

            let new_op = Token {
                token_type: match op.token_type {
                    TokT::PlusEqual => TokT::Plus,
                    TokT::MinusEqual => TokT::Minus,
                    TokT::StarEqual => TokT::Star,
                    TokT::SlashEqual => TokT::Slash,
                    TokT::PercentEqual => TokT::Percent,
                    _ => unreachable!(),
                },
                ..op.clone()
            };

            let binary = Expr::Binary {
                left: Box::new(left),
                operator: new_op,
                right: Box::new(right),
            };

            let target = self.handle_assigment(expr, binary)?;
            return Ok(target);
        }
        Ok(expr)
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
        } else if let Expr::GetIndex {
            safe,
            object,
            index,
        } = expr
        {
            Ok(Expr::SetIndex {
                safe,
                object,
                index,
                value: Box::new(value),
            })
        } else {
            Err(self.error_previous("Invalid assignment target."))
        }
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
        let mut left = self.equality()?;
        while match_token_type!(self, TokT::And) {
            let operator = self.previous_token.clone();
            let right = self.equality()?;
            left = Expr::Logical {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }
        Ok(left)
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
        while match_token_type!(self, TokT::Star, TokT::Slash, TokT::Percent) {
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
        if match_token_type!(self, TokT::Bang, TokT::Minus, TokT::Try) {
            let op = self.previous_token.clone();
            let expr = self.unary()?;
            Ok(Expr::Unary {
                operator: op,
                expression: Box::new(expr),
            })
        } else {
            self.power()
        }
    }

    fn power(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let base = self.parser_is_type()?;
        if match_token_type!(self, TokT::StarStar) {
            let op = self.previous_token.clone();
            let exponent = self.unary()?;
            Ok(Expr::Binary {
                left: Box::new(base),
                operator: op,
                right: Box::new(exponent),
            })
        } else {
            Ok(base)
        }
    }

    fn parser_is_type(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let mut expr = self.call()?;
        if match_token_type!(self, TokT::Is) {
            self.consume(TokT::Identifier, "Expected typename after is.")?;
            let type_name = self.previous_token.clone();
            expr = Expr::Is {
                expression: Box::new(expr),
                type_name,
            }
        }
        Ok(expr)
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
            } else if check_token_type!(self, TokT::Dot) && check_next_token_type!(self, TokT::Less)
            {
                self.consume(TokT::Dot, "unreachable")?;
                let generics = self.parse_generic_arguments()?;
                expr = Expr::TypeSpecialization {
                    callee: Box::new(expr),
                    generics,
                };
            } else if check_token_type!(self, TokT::Question)
                && check_next_token_type!(self, TokT::LeftBracket)
            {
                self.consume(TokT::Question, "unreachable")?;
                self.consume(TokT::LeftBracket, "unreachable")?;

                let index = self.expression()?;
                self.consume(TokT::RightBracket, "Expected ']' after index.")?;
                expr = Expr::GetIndex {
                    safe: true,
                    object: Box::new(expr),
                    index: Box::new(index),
                };
            } else if match_token_type!(self, TokT::LeftBracket) {
                let index = self.expression()?;
                self.consume(TokT::RightBracket, "Expected ']' after index.")?;
                expr = Expr::GetIndex {
                    safe: false,
                    object: Box::new(expr),
                    index: Box::new(index),
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
                if match_token_type!(self, TokT::Number) {
                    let idx = self.previous_token.clone();
                    expr = Expr::Get {
                        safe: true,
                        object: Box::new(expr),
                        field: idx,
                    };
                    continue;
                }

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
                    operator: self.previous_token.clone(),
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
            if check_token_type!(self, TokT::RightParen) {
                self.errors.push(ParserError::UnexpectedToken {
                    found: self.previous_token.clone(),
                    message: "Trailing comma is not allowed here".to_string(),
                });
                break;
            }
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
                if let Some(num) = parse_number(self.previous_token.lexeme) {
                    self.literal(Literal::Number(num))
                } else {
                    Err(self.error_current("Invalid number literal."))
                }
            }
            TokT::String => {
                // Strip surrounding quotes.
                let inner = &self.previous_token.lexeme[1..self.previous_token.lexeme.len() - 1];
                let processed = process_escapes(inner).map_err(|msg| self.error_previous(msg))?;
                self.literal(Literal::String(processed))
            }
            TokT::PartialString => self.parse_string_interp(),
            TokT::RawString => {
                let content = process_raw_string(self.previous_token.lexeme);
                self.literal(Literal::String(content))
            }
            TokT::Identifier => Ok(Expr::Variable {
                name: self.previous_token.clone(),
            }),
            TokT::Self_ => Ok(Expr::Variable {
                name: self.previous_token.clone(),
            }),
            TokT::LeftBracket => {
                let bracket_token = self.previous_token.clone();
                if match_token_type!(self, TokT::RightBracket) {
                    return Ok(Expr::List {
                        elements: vec![],
                        bracket_token,
                    });
                } else if match_token_type!(self, TokT::Colon) {
                    self.consume(TokT::RightBracket, "Expected ']' after : in map literal.")?;
                    return Ok(Expr::Map {
                        kv_pairs: vec![],
                        bracket_token,
                    });
                }

                let first_expr = self.expression()?;
                // Map [key:value]
                if match_token_type!(self, TokT::Colon) {
                    let first_val = self.expression()?;
                    let mut kv_pairs = vec![(first_expr, first_val)];

                    while match_token_type!(self, TokT::Comma) {
                        if check_token_type!(self, TokT::RightBracket) {
                            break;
                        }
                        let key = self.expression()?;
                        self.consume(TokT::Colon, "Expected ':' separating key and value in map.")?;
                        let value = self.expression()?;
                        kv_pairs.push((key, value));
                    }
                    self.consume(TokT::RightBracket, "Expected ']' after map entries.")?;
                    Ok(Expr::Map {
                        kv_pairs,
                        bracket_token,
                    })
                } else {
                    let mut elements = vec![first_expr];
                    while match_token_type!(self, TokT::Comma) {
                        if check_token_type!(self, TokT::RightBracket) {
                            break;
                        }
                        elements.push(self.expression()?);
                    }
                    self.consume(TokT::RightBracket, "Expected ']' after list elements.")?;
                    Ok(Expr::List {
                        elements,
                        bracket_token,
                    })
                }
            }
            TokT::LeftParen => {
                let expr = self.expression()?;
                let mut tuple_args = vec![];
                if match_token_type!(self, TokT::Comma) {
                    tuple_args.push(expr);
                    loop {
                        let expr = self.expression()?;
                        tuple_args.push(expr);
                        if !match_token_type!(self, TokT::Comma) {
                            break;
                        }
                    }
                    self.consume(TokT::RightParen, "Expected ')' to close tuple")?;

                    return Ok(Tuple {
                        elements: tuple_args,
                    });
                }

                self.consume(TokT::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::Grouping {
                    expression: Box::new(expr),
                })
            }
            TokT::LeftBrace => self.parse_block_expr(),
            TokT::If => self.parse_if_expr(),
            TokT::Match => self.parse_match_expr(),
            TokT::Pipe => self.parse_lambda(),
            _ => Err(self.error_previous(format!(
                "Expected expression, but found '{}'.",
                self.previous_token.lexeme
            ))),
        }
    }
    fn literal(&mut self, literal: Literal) -> Result<Expr<'src>, ParserError<'src>> {
        Ok(Expr::Literal {
            literal,
            span: self.previous_token.span,
        })
    }

    /// `{` already consumed. Parses a block expression: `{ stmt* expr? }`.
    /// Declarations (let, func, struct, …) and while/return are always statements.
    /// Any other item without a trailing `;` before `}` becomes the tail expression.
    fn parse_block_expr(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let brace_token = self.previous_token.clone();
        let mut body: Vec<Stmt<'src>> = vec![];
        let mut tail: Option<Box<Expr<'src>>> = None;

        loop {
            if check_token_type!(self, TokT::RightBrace) || check_token_type!(self, TokT::EOF) {
                break;
            }

            // Items that are only ever statements — delegate to the full declaration path.
            if self.is_stmt_start() {
                match self.declaration() {
                    Ok(stmt) => body.push(stmt),
                    Err(e) => {
                        self.synchronize();
                        self.errors.push(e);
                        if check_token_type!(self, TokT::EOF) {
                            break;
                        }
                    }
                }
                continue;
            }

            // Limit brace expressions in mid positions
            let expr = match self.current_token.token_type {
                TokT::LeftBrace => {
                    self.consume(TokT::LeftBrace, "Expected '{' to open block expression.")?;
                    self.parse_block_expr()
                }
                TokT::If => {
                    self.consume(TokT::If, "Expected 'if' to open if expression.")?;
                    self.parse_if_expr()
                }
                TokT::Match => {
                    self.consume(TokT::Match, "Expected 'match' to open match expr ")?;
                    self.parse_match_expr()
                }
                _ => self.expression(),
            };

            match expr {
                Ok(expr) => {
                    let is_brace_expr = self.previous_token.token_type == TokT::RightBrace;
                    let is_final = self.current_token.token_type == TokT::RightBrace;
                    let has_semicolon = match_token_type!(self, TokT::Semicolon);
                    // } + semicolon -> discard
                    // expr + ; -> stmt
                    // expr + final -> tail
                    // expr + !final -> error
                    // } + final -> tail
                    match (is_final, has_semicolon) {
                        (true, false) => tail = Some(Box::new(expr)),
                        (_, true) => body.push(Stmt::Expression(expr)),
                        (false, false) => {
                            if is_brace_expr {
                                // TODO how to classify if else stmt like expr
                                body.push(Stmt::Expression(expr));
                            } else {
                                self.error_previous("Expected ';' after expression.");
                            }
                        }
                    }
                }
                Err(e) => {
                    self.synchronize();
                    self.errors.push(e);
                    if check_token_type!(self, TokT::EOF) {
                        break;
                    }
                }
            }
        }

        self.consume(TokT::RightBrace, "Expected '}' to close block expression.")?;
        Ok(Expr::Block {
            brace_token,
            body,
            tail,
        })
    }

    /// `if` already consumed. Parses `if cond { expr } (else ({ expr } | if ...))?`.
    fn parse_if_expr(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let condition = self.expression()?;
        // Consume `{` and parse the then-branch as a block expression.
        self.consume(TokT::LeftBrace, "Expected '{' after if condition.")?;
        let then_branch = self.parse_block_expr()?;

        let else_branch = if match_token_type!(self, TokT::Else) {
            if match_token_type!(self, TokT::If) {
                Some(Box::new(self.parse_if_expr()?))
            } else {
                self.consume(TokT::LeftBrace, "Expected '{' after else.")?;
                Some(Box::new(self.parse_block_expr()?))
            }
        } else {
            None
        };

        Ok(Expr::IfExpr {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    /// `match` already consumed. Parses `match expr { (pattern => expr ,?)* }`.
    fn parse_match_expr(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let value = self.expression()?;
        self.consume(TokT::LeftBrace, "Expected '{' after match value.")?;
        let mut arms: Vec<ExprMatchArm<'src>> = vec![];

        while !check_token_type!(self, TokT::RightBrace) && !check_token_type!(self, TokT::EOF) {
            let pattern = self.pattern()?;
            self.consume(TokT::Arrow, "Expected '=>' after match pattern.")?;
            let body = self.expression()?;
            arms.push(ExprMatchArm { pattern, body });
            // Optional comma between arms; allows trailing comma.
            match_token_type!(self, TokT::Comma);
        }

        self.consume(TokT::RightBrace, "Expected '}' after match arms.")?;
        Ok(Expr::MatchExpr {
            value: Box::new(value),
            arms,
        })
    }

    /// `|` already consumed. Parses `|param*| body_expr`.
    /// Zero-param form: `|| expr` (the opening `|` was consumed, next is `|`).
    fn parse_lambda(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let pipe_token = self.previous_token.clone();
        let mut params: Vec<(Token<'src>, TypeAst<'src>)> = vec![];

        if !match_token_type!(self, TokT::Pipe) {
            // Parse parameter list until closing `|`.
            while !check_token_type!(self, TokT::Pipe) && !check_token_type!(self, TokT::EOF) {
                // Name: identifier or `_`.
                if !match_token_type!(self, TokT::Identifier) {
                    return Err(self.error_current("Expected parameter name in lambda."));
                }
                let name = self.previous_token.clone();
                let type_ann = if match_token_type!(self, TokT::Colon) {
                    self.type_block()?
                } else {
                    TypeAst::Infer
                };
                params.push((name, type_ann));
                if !match_token_type!(self, TokT::Comma) {
                    break;
                }
            }
            self.consume(TokT::Pipe, "Expected '|' to close lambda parameters.")?;
        }

        // Body: any expression (block expression included).
        let body = self.expression()?;
        Ok(Expr::Lambda {
            params,
            body: Box::new(body),
            pipe_token,
        })
    }

    /// Parses a `PartialString` token (already consumed) through to the
    /// matching `String` terminator, collecting literal parts and `${expr}` segments.
    fn parse_string_interp(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let start_span = self.previous_token.span;

        // PartialString lexeme: `"<content>` (opening quote, then raw content up to `${`).
        let first_raw = &self.previous_token.lexeme[1..]; // strip opening "
        let first_text = process_escapes(first_raw).map_err(|msg| self.error_previous(msg))?;

        let mut parts: Vec<StringPart<'src>> = Vec::new();
        if !first_text.is_empty() {
            parts.push(StringPart::Literal(first_text));
        }

        loop {
            // Parse the interpolated expression.
            let expr = self.expression()?;
            parts.push(StringPart::Expr(Box::new(expr)));

            if match_token_type!(self, TokT::String) {
                // String lexeme: `<content>"`.
                let end_raw = self.previous_token.lexeme;
                let end_raw = &end_raw[..end_raw.len() - 1]; // strip closing "
                let end_text = process_escapes(end_raw).map_err(|msg| self.error_previous(msg))?;
                if !end_text.is_empty() {
                    parts.push(StringPart::Literal(end_text));
                }
                let span = start_span.merge(self.previous_token.span);
                return Ok(Expr::StringInterp { parts, span });
            } else if match_token_type!(self, TokT::PartialString) {
                let mid_text = process_escapes(self.previous_token.lexeme)
                    .map_err(|msg| self.error_previous(msg))?;
                if !mid_text.is_empty() {
                    parts.push(StringPart::Literal(mid_text));
                }
                // Continue to parse the next interpolated expression.
            } else {
                return Err(
                    self.error_current("Expected string interpolation to continue or close.")
                );
            }
        }
    }
}
