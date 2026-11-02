//! Control-flow expressions: `if`/`else`, `match` (arms and patterns) and `return`.

use crate::parser::ast::{Binding, BindingKind, Expr, ExprKind, MatchArm, Pattern, PatternKind};
use crate::parser::declarations::{BINDING_START, STRUCT_DESTRUCTURE, WILDCARD};
use crate::parser::error::{ParenContext, ParseError, ParseErrorKind};
use crate::parser::expressions::EXPR_START;
use crate::parser::lists::{Delim, ListSpec};
use crate::parser::recovery::RECOVERY_STOP;
use crate::parser::{PResult, Parser, TokT};

impl<'src> Parser<'src> {
    pub(super) fn if_expr(&mut self) -> PResult<Expr<'src>> {
        let start = self.advance_kind(TokT::If).span;
        let condition = self.expression()?;
        let then_branch = self.block(true, "if condition")?.into_expr();

        let else_branch = if self.try_consume(TokT::Else) {
            if self.check(TokT::If) {
                Some(self.if_expr()?.boxed())
            } else {
                Some(self.block(true, "`else`")?.into_expr().boxed())
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

    pub(super) fn pattern(&mut self) -> PResult<Pattern<'src>> {
        let start = self.start_span();
        if self.check(TokT::Identifier) && !self.check_next(TokT::Dot) {
            let name = self.advance_ident();
            let kind = if name.name == WILDCARD {
                PatternKind::Wildcard
            } else {
                PatternKind::Name(name)
            };
            return Ok(Pattern {
                kind,
                span: name.span,
            });
        }

        let enum_name = self.check(TokT::Identifier).then(|| self.advance_ident());
        self.expect(TokT::Dot, "enum name")?;
        let variant = self.expect_ident("`.`")?;

        let payload = if self.check(TokT::LeftParen) {
            // `(` is consumed first: telling `.V(x: a)` from `.V(a)` needs the two tokens after it.
            let open = self.advance().span;
            if self.check(TokT::Colon)
                || (self.check(TokT::Identifier) && self.check_next(TokT::Colon))
            {
                let fields = self.delimited_after(open, STRUCT_DESTRUCTURE, Self::field_binding);
                Some(Binding {
                    span: variant.span.merge(fields.close),
                    kind: BindingKind::Struct {
                        name: variant,
                        fields: fields.items,
                    },
                })
            } else {
                let mut bindings =
                    self.delimited_after(open, ListSpec::paren("binding", BINDING_START), |p| {
                        p.binding("`(`")
                    });
                match bindings.items.len() {
                    0 => {
                        if !bindings.failed {
                            self.errors.push(ParseError::new(
                                ParseErrorKind::EmptyParens(ParenContext::PatternPayload),
                                bindings.span(),
                            ));
                        }
                        None
                    }
                    1 => bindings.items.pop(),
                    _ => Some(Binding {
                        span: bindings.span(),
                        kind: BindingKind::Tuple(bindings.items),
                    }),
                }
            }
        } else {
            None
        };

        Ok(Pattern {
            kind: PatternKind::Variant {
                enum_name,
                variant,
                payload,
            },
            span: self.span_from(start),
        })
    }
}
