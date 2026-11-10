//! Control-flow: `if`/`else`, `match`, while and `return`.

use crate::parser::ast::{Expr, ExprKind, MatchArm, Stmt, While};
use crate::parser::expressions::EXPR_START;
use crate::parser::lists::Delim;
use crate::parser::recovery::RECOVERY_STOP;
use crate::parser::statements::StmtCtx;
use crate::parser::{PResult, Parser, TokT};

impl<'src> Parser<'src> {
    pub(super) fn if_expr(&mut self) -> PResult<Expr<'src>> {
        let start = self.advance_kind(TokT::If).span;
        let condition = self.expression()?;
        let then_branch = self.block(StmtCtx::Value, "if condition")?.into_expr();

        let else_branch = if self.try_consume(TokT::Else) {
            if self.check(TokT::If) {
                Some(self.if_expr()?.boxed())
            } else {
                Some(self.block(StmtCtx::Value, "`else`")?.into_expr().boxed())
            }
        } else {
            None
        };

        Ok(Expr::new(
            ExprKind::If {
                condition: condition.boxed(),
                then_branch: then_branch.boxed(),
                else_branch,
            },
            self.span_from(start),
        ))
    }

    pub(super) fn while_statement(&mut self) -> PResult<Stmt<'src>> {
        let start = self.advance_kind(TokT::While).span;
        let condition = self.expression()?;
        let body = self.block(StmtCtx::Body, "while condition")?.into_body();
        Ok(Stmt::While(Box::new(While {
            condition,
            body,
            span: self.span_from(start),
        })))
    }

    /// `return` with an optional value: there is one exactly when the next token can start an expression.
    pub(super) fn return_expr(&mut self) -> PResult<Expr<'src>> {
        let start = self.advance_kind(TokT::Return).span;
        let value = if self.check_any(EXPR_START) {
            Some(self.expression()?.boxed())
        } else {
            None
        };
        Ok(Expr::new(ExprKind::Return(value), self.span_from(start)))
    }

    /// A recovery point: a bad arm is reported and skipped to the next `,` or the closing `}`.
    pub(super) fn match_expr(&mut self) -> PResult<Expr<'src>> {
        let start = self.advance_kind(TokT::Match).span;
        let scrutinee = self.expression()?;
        let open = self.expect(TokT::LeftBrace, "match value")?.span;
        let mut arms: Vec<MatchArm<'src>> = vec![];

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
        Ok(Expr::new(
            ExprKind::Match {
                scrutinee: scrutinee.boxed(),
                arms,
            },
            self.span_from(start),
        ))
    }

    /// `pattern => body`, plus its `,`: optional after a block body, otherwise required unless the arm is last.
    fn match_arm(&mut self) -> PResult<MatchArm<'src>> {
        let pattern = self.pattern()?;
        self.expect(TokT::Arrow, "match pattern")?;
        let block_body = self.check(TokT::LeftBrace);
        let body = if block_body {
            self.block_expr()?
        } else {
            self.expression()?
        };
        let span = pattern.span.merge(body.span);
        if block_body {
            self.try_consume(TokT::Comma);
        } else if !self.check(TokT::RightBrace) {
            self.expect(TokT::Comma, "match arm")?;
        }
        Ok(MatchArm {
            pattern,
            body,
            span,
        })
    }
}
