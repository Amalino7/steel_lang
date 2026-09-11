use crate::parser::ast::{Binding, Expr, ExprMatchArm, Literal, Pattern, Stmt};
use crate::parser::error::ParserError;
use crate::parser::{check_next_token_type, check_token_type, match_token_type, Parser, TokT};

impl<'src> Parser<'src> {
    pub(super) fn parse_block_expr(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
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
                    self.consume(TokT::LeftBrace, "Unreachable!")?;
                    self.parse_block_expr()
                }
                TokT::If => {
                    self.consume(TokT::If, "Unreachable!")?;
                    self.parse_if_expr()
                }
                TokT::Match => {
                    self.consume(TokT::Match, "Unreachable!")?;
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
                                // Brace-terminated expressions (if/match/{}) in ambiguous positions
                                // are treated like statements
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

    pub(super) fn parse_if_expr(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
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
    pub(super) fn parse_return_expr(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let keyword = self.previous_token.clone();
        let value = if !check_token_type!(
            self,
            TokT::Semicolon,
            TokT::RightBrace,
            TokT::EOF,
            TokT::Comma,
            TokT::RightParen
        ) {
            self.expression()?
        } else {
            Expr::Literal {
                literal: Literal::Void,
                span: self.previous_token.span,
            }
        };
        Ok(Expr::Return {
            keyword,
            value: Box::new(value),
        })
    }

    pub(super) fn parse_match_expr(&mut self) -> Result<Expr<'src>, ParserError<'src>> {
        let value = self.expression()?;
        self.consume(TokT::LeftBrace, "Expected '{' after match value.")?;
        let mut arms: Vec<ExprMatchArm<'src>> = vec![];

        while !check_token_type!(self, TokT::RightBrace) && !check_token_type!(self, TokT::EOF) {
            let pattern = self.pattern()?;
            self.consume(TokT::Arrow, "Expected '=>' after match pattern.")?;
            // Brace-terminated expressions are parsed directly so the trailing `}`
            let mut is_block = false;
            let body = match self.current_token.token_type {
                TokT::LeftBrace => {
                    self.consume(TokT::LeftBrace, "unreachable")?;
                    is_block = true;
                    self.parse_block_expr()?
                }
                _ => self.expression()?,
            };
            arms.push(ExprMatchArm { pattern, body });
            if is_block {
                // Blocks get optional commas
                match_token_type!(self, TokT::Comma);
            } else if !check_token_type!(self, TokT::RightBrace) {
                // Non-block expressions get a mandatory comma unless final
                self.consume(TokT::Comma, "Expected ',' after match arm.")?;
            }
        }

        self.consume(TokT::RightBrace, "Expected '}' after match arms.")?;
        Ok(Expr::MatchExpr {
            value: Box::new(value),
            arms,
        })
    }

    pub(super) fn pattern(&mut self) -> Result<Pattern<'src>, ParserError<'src>> {
        if check_token_type!(self, TokT::Identifier) && !check_next_token_type!(self, TokT::Dot) {
            self.consume(TokT::Identifier, "Expected variable name.")?;
            let var_name = self.previous_token.clone();
            return Ok(Pattern::Variable(var_name));
        }

        let enum_name = if match_token_type!(self, TokT::Identifier) {
            Some(self.previous_token.clone())
        } else {
            None
        };
        self.consume(
            TokT::Dot,
            "Expected '.' as pattern start or between enum name and variant ",
        )?;

        self.consume(TokT::Identifier, "Expected variant name.")?;
        let variant_name = self.previous_token.clone();

        let bind = if match_token_type!(self, TokT::LeftParen) {
            // Struct like case, :x or val:val
            if check_token_type!(self, TokT::Colon) || check_next_token_type!(self, TokT::Colon) {
                Some(self.parse_destructure(variant_name.clone())?)
            } else {
                let mut bindings = vec![];
                while !check_token_type!(self, TokT::RightParen) {
                    bindings.push(self.parse_binding()?);
                    match_token_type!(self, TokT::Comma);
                }
                self.consume(TokT::RightParen, "Expected ')' pattern.")?;

                if bindings.is_empty() {
                    None
                } else if bindings.len() == 1 {
                    Some(bindings.remove(0))
                } else {
                    Some(Binding::Tuple { fields: bindings })
                }
            }
        } else {
            None
        };

        Ok(Pattern::Named {
            enum_name,
            variant_name,
            bind,
        })
    }
}
