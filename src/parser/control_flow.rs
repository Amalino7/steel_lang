//! Control-flow expressions: `if`/`else`, `match` (arms and patterns) and `return`.

use crate::parser::ast::{Binding, Expr, ExprMatchArm, Literal, Pattern};
use crate::parser::declarations::{BINDING_START, STRUCT_DESTRUCTURE};
use crate::parser::error::ParserError;
use crate::parser::expressions::EXPR_START;
use crate::parser::lists::{Delim, ListSpec};
use crate::parser::recovery::RECOVERY_STOP;
use crate::parser::{PResult, Parser, TokT};

impl<'src> Parser<'src> {
    pub(super) fn if_expr(&mut self) -> PResult<'src, Expr<'src>> {
        self.advance_kind(TokT::If);
        let condition = self.expression()?;
        let then_branch = self.block(true, "if condition")?.into_expr();

        let else_branch = if self.try_consume(TokT::Else) {
            if self.check(TokT::If) {
                Some(Box::new(self.if_expr()?))
            } else {
                Some(Box::new(self.block(true, "'else'")?.into_expr()))
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

    /// `return` with an optional value: there is one exactly when the next token can start an expression.
    pub(super) fn return_expr(&mut self) -> PResult<'src, Expr<'src>> {
        let keyword = self.advance_kind(TokT::Return);
        let value = if self.check_any(EXPR_START) {
            self.expression()?
        } else {
            Expr::Literal {
                literal: Literal::Void,
                span: keyword.span,
            }
        };
        Ok(Expr::Return {
            keyword,
            value: Box::new(value),
        })
    }

    /// A recovery point: a bad arm is reported and skipped to the next `,` or the closing `}`.
    pub(super) fn match_expr(&mut self) -> PResult<'src, Expr<'src>> {
        self.advance_kind(TokT::Match);
        let value = self.expression()?;
        let open = self.expect(TokT::LeftBrace, "match value")?.span;
        let mut arms: Vec<ExprMatchArm<'src>> = vec![];

        while !self.check(TokT::RightBrace) && !self.at_eof() {
            let before = self.pos;
            match self.match_arm() {
                Ok(arm) => arms.push(arm),
                Err(e) => {
                    self.errors.push(e);
                    self.skip_to(&[&[TokT::Comma], RECOVERY_STOP]);
                    self.try_consume(TokT::Comma);
                    if self.pos == before {
                        // Stuck on a statement boundary or a stray closer: the match is unclosed.
                        break;
                    }
                }
            }
        }

        self.close_delimiter(open, Delim::Brace);
        Ok(Expr::MatchExpr {
            value: Box::new(value),
            arms,
        })
    }

    /// `pattern => body`, plus its `,`: optional after a block body, otherwise required unless the arm is last.
    fn match_arm(&mut self) -> PResult<'src, ExprMatchArm<'src>> {
        let pattern = self.pattern()?;
        self.expect(TokT::Arrow, "match pattern")?;
        if self.check(TokT::LeftBrace) {
            let body = self.block_expr()?;
            self.try_consume(TokT::Comma);
            return Ok(ExprMatchArm { pattern, body });
        }
        let body = self.expression()?;
        if !self.check(TokT::RightBrace) {
            self.expect(TokT::Comma, "match arm")?;
        }
        Ok(ExprMatchArm { pattern, body })
    }

    pub(super) fn pattern(&mut self) -> PResult<'src, Pattern<'src>> {
        if self.check(TokT::Identifier) && !self.check_next(TokT::Dot) {
            return Ok(Pattern::Variable(self.advance()));
        }

        let enum_name = self.try_consume(TokT::Identifier).then_some(self.previous);
        self.expect(TokT::Dot, "enum name")?;
        let variant_name = self.expect_ident("'.'")?;

        let bind = if self.check(TokT::LeftParen) {
            // `(` is consumed first: telling `.V(x: a)` from `.V(a)` needs the two tokens after it.
            let open = self.advance().span;
            if self.check(TokT::Colon)
                || (self.check(TokT::Identifier) && self.check_next(TokT::Colon))
            {
                let fields =
                    self.delimited_after(open, STRUCT_DESTRUCTURE, Self::destructure_field);
                Some(Binding::Struct {
                    name: variant_name,
                    fields: fields.items,
                })
            } else {
                let mut bindings =
                    self.delimited_after(open, ListSpec::paren("binding", BINDING_START), |p| {
                        p.parse_binding("'('")
                    });
                match bindings.items.len() {
                    0 => {
                        if !bindings.failed {
                            self.errors.push(ParserError::Invalid {
                                span: bindings.open.to(bindings.close),
                                message: "Empty pattern payload: remove the `()`.".to_string(),
                            });
                        }
                        None
                    }
                    1 => bindings.items.pop(),
                    _ => Some(Binding::Tuple {
                        fields: bindings.items,
                    }),
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
