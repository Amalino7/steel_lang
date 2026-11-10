//! Expressions: precedence climbing from assignment down to `primary`, postfix chains (calls, indexing, field
//! access, turbofish), literals, collections, lambdas and string interpolation.

use crate::parser::ast::{
    BinaryOp, CallArg, Expr, ExprKind, Ident, LambdaParam, Literal, LogicalOp, MapEntry, Member,
    Spanned, StringPart, UnaryOp,
};
use crate::parser::error::{Expected, NumberError, ParenContext, ParseErrorKind};
use crate::parser::lists::{Delim, ListSpec};
use crate::parser::literals::{
    parse_number, process_escapes, process_raw_string, strip_underscores,
};
use crate::parser::{PResult, Parser, TokT};
use crate::scanner::Span;
use crate::scanner::token::Token;
use std::ops::Range;

/// FIRST set of an expression: the prefix operators `unary` takes plus every token `primary` dispatches on.
/// Keep it in sync with both. A token outside it can't begin an expression.
pub(super) const EXPR_START: &[TokT] = &[
    TokT::Bang,
    TokT::Minus,
    TokT::Try,
    TokT::True,
    TokT::False,
    TokT::Nil,
    TokT::Number,
    TokT::String,
    TokT::PartialString,
    TokT::RawString,
    TokT::Identifier,
    TokT::Self_,
    TokT::LeftBracket,
    TokT::LeftParen,
    TokT::LeftBrace,
    TokT::If,
    TokT::Match,
    TokT::Pipe,
    TokT::Return,
];

impl<'src> Parser<'src> {
    pub(crate) fn expression(&mut self) -> PResult<Expr<'src>> {
        self.assignment()
    }

    fn assignment(&mut self) -> PResult<Expr<'src>> {
        let expr = self.null_coalescing()?;

        if self.try_consume(TokT::Equal) {
            let value = self.assignment()?;
            return Ok(self.assignment_target(expr, value));
        }

        let compound_kind = match self.current.kind {
            TokT::PlusEqual => Some(BinaryOp::Add),
            TokT::MinusEqual => Some(BinaryOp::Subtract),
            TokT::StarEqual => Some(BinaryOp::Multiply),
            TokT::SlashEqual => Some(BinaryOp::Divide),
            TokT::PercentEqual => Some(BinaryOp::Modulo),
            _ => None,
        };

        if let Some(op) = compound_kind {
            let op = Spanned {
                node: op,
                span: self.advance().span,
            };
            let right = self.assignment()?;
            let value = binary(expr.clone(), op, right);
            return Ok(self.assignment_target(expr, value));
        }
        Ok(expr)
    }

    /// Turns the parsed left-hand side into an assignment. An invalid target is reported and parsing goes on.
    fn assignment_target(&mut self, target: Expr<'src>, value: Expr<'src>) -> Expr<'src> {
        let span = target.span.merge(value.span);
        let value = value.boxed();
        let kind = match target.kind {
            ExprKind::Variable(name) => ExprKind::Assign {
                target: name,
                value,
            },
            ExprKind::Field {
                object,
                safe,
                member,
            } => ExprKind::SetField {
                object,
                safe,
                member,
                value,
            },
            ExprKind::Index {
                object,
                safe,
                index,
            } => ExprKind::SetIndex {
                object,
                safe,
                index,
                value,
            },
            _ => {
                self.report(ParseErrorKind::InvalidAssignmentTarget, target.span);
                ExprKind::Error
            }
        };
        Expr::new(kind, span)
    }

    fn null_coalescing(&mut self) -> PResult<Expr<'src>> {
        let mut expr = self.logical_or()?;
        while self.try_consume(TokT::QuestionQuestion) {
            let op = self.previous.span;
            let right = self.logical_or()?;
            let span = expr.span.merge(right.span);
            expr = Expr::new(
                ExprKind::Coalesce {
                    left: expr.boxed(),
                    op,
                    right: right.boxed(),
                },
                span,
            );
        }
        Ok(expr)
    }

    fn logical_or(&mut self) -> PResult<Expr<'src>> {
        self.logical_level(TokT::Or, LogicalOp::Or, Self::logical_and)
    }

    fn logical_and(&mut self) -> PResult<Expr<'src>> {
        self.logical_level(TokT::And, LogicalOp::And, Self::equality)
    }

    /// One left-associative logical level: `next (kind next)*`.
    fn logical_level(
        &mut self,
        kind: TokT,
        op: LogicalOp,
        next: impl Fn(&mut Self) -> PResult<Expr<'src>>,
    ) -> PResult<Expr<'src>> {
        let mut expr = next(self)?;
        while self.try_consume(kind) {
            let op = Spanned {
                node: op,
                span: self.previous.span,
            };
            let right = next(self)?;
            let span = expr.span.merge(right.span);
            expr = Expr::new(
                ExprKind::Logical {
                    left: expr.boxed(),
                    op,
                    right: right.boxed(),
                },
                span,
            );
        }
        Ok(expr)
    }

    /// One left-associative binary precedence level: `next (op next)*` for `op` in `ops`.
    fn binary_level(
        &mut self,
        ops: &[TokT],
        next: impl Fn(&mut Self) -> PResult<Expr<'src>>,
    ) -> PResult<Expr<'src>> {
        let mut expr = next(self)?;
        while self.try_consume_any(ops) {
            let op = self.binary_op();
            let right = next(self)?;
            expr = binary(expr, op, right);
        }
        Ok(expr)
    }

    /// The binary operator just consumed.
    fn binary_op(&self) -> Spanned<BinaryOp> {
        let operation = match self.previous.kind {
            TokT::Plus => BinaryOp::Add,
            TokT::Minus => BinaryOp::Subtract,
            TokT::Star => BinaryOp::Multiply,
            TokT::Slash => BinaryOp::Divide,
            TokT::Percent => BinaryOp::Modulo,
            TokT::StarStar => BinaryOp::Power,
            TokT::EqualEqual => BinaryOp::Equal,
            TokT::BangEqual => BinaryOp::NotEqual,
            TokT::Less => BinaryOp::Less,
            TokT::LessEqual => BinaryOp::LessEqual,
            TokT::Greater => BinaryOp::Greater,
            TokT::GreaterEqual => BinaryOp::GreaterEqual,
            other => unreachable!("not a binary operator: {other:?}"),
        };

        Spanned {
            node: operation,
            span: self.previous.span,
        }
    }

    fn equality(&mut self) -> PResult<Expr<'src>> {
        self.binary_level(&[TokT::EqualEqual, TokT::BangEqual], Self::comparison)
    }

    fn comparison(&mut self) -> PResult<Expr<'src>> {
        self.binary_level(
            &[
                TokT::Greater,
                TokT::GreaterEqual,
                TokT::Less,
                TokT::LessEqual,
            ],
            Self::term,
        )
    }

    fn term(&mut self) -> PResult<Expr<'src>> {
        self.binary_level(&[TokT::Minus, TokT::Plus], Self::factor)
    }

    fn factor(&mut self) -> PResult<Expr<'src>> {
        self.binary_level(&[TokT::Star, TokT::Slash, TokT::Percent], Self::unary)
    }

    fn unary(&mut self) -> PResult<Expr<'src>> {
        let start = self.start_span();
        let op = match self.current.kind {
            TokT::Bang => UnaryOp::Not,
            TokT::Minus => UnaryOp::Negate,
            TokT::Try => {
                let keyword = self.advance().span;
                let operand = self.unary()?;
                return Ok(Expr::new(
                    ExprKind::Try {
                        keyword,
                        operand: operand.boxed(),
                    },
                    self.span_from(start),
                ));
            }
            _ => return self.power(),
        };
        let op = Spanned {
            node: op,
            span: self.advance().span,
        };
        let operand = self.unary()?;
        Ok(Expr::new(
            ExprKind::Unary {
                op,
                operand: operand.boxed(),
            },
            self.span_from(start),
        ))
    }

    fn power(&mut self) -> PResult<Expr<'src>> {
        let base = self.type_test()?;
        if self.try_consume(TokT::StarStar) {
            let op = self.binary_op();
            let exponent = self.unary()?;
            Ok(binary(base, op, exponent))
        } else {
            Ok(base)
        }
    }

    /// `expr is Variant`.
    fn type_test(&mut self) -> PResult<Expr<'src>> {
        let expr = self.call()?;
        if self.try_consume(TokT::Is) {
            let variant = self.expect_ident("`is`")?;
            let span = expr.span.merge(variant.span);
            return Ok(Expr::new(
                ExprKind::Is {
                    operand: expr.boxed(),
                    variant,
                },
                span,
            ));
        }
        Ok(expr)
    }

    /// `primary` followed by any chain of calls, indexing, field/tuple access, turbofish and `!`. Each link's
    /// span runs from the start of the chain to its own last token.
    fn call(&mut self) -> PResult<Expr<'src>> {
        let mut expr = self.primary()?;
        loop {
            let start = expr.span;
            let (kind, end) = match self.current.kind {
                TokT::LeftParen | TokT::QuestionParen => {
                    let open = self.advance();
                    let args = self.delimited_after(
                        open.span,
                        ListSpec::paren("argument", EXPR_START),
                        Self::call_arg,
                    );
                    let end = args.close;
                    let kind = ExprKind::Call {
                        callee: expr.boxed(),
                        safe: (open.kind == TokT::QuestionParen).then_some(open.span),
                        args: args.into_ast(),
                    };
                    (kind, end)
                }
                TokT::Dot if self.check_next(TokT::Less) => {
                    self.advance();
                    let type_args = self.type_arg_list();
                    let end = type_args.close;
                    let kind = ExprKind::TypeSpecialization {
                        callee: expr.boxed(),
                        type_args: type_args.into_ast(),
                    };
                    (kind, end)
                }
                TokT::Question if self.check_next(TokT::LeftBracket) => {
                    let safe = self.advance().span;
                    self.index(expr, Some(safe))?
                }
                TokT::LeftBracket => self.index(expr, None)?,
                TokT::Dot | TokT::QuestionDot => {
                    let dot = self.advance();
                    let safe = (dot.kind == TokT::QuestionDot).then_some(dot.span);
                    let member = if self.check(TokT::Number) {
                        match self.tuple_index() {
                            Some(index) => Member::Index(index),
                            None => {
                                expr = Expr::new(ExprKind::Error, start.merge(self.previous.span));
                                continue;
                            }
                        }
                    } else {
                        Member::Named(self.expect_ident("member access")?)
                    };
                    let kind = ExprKind::Field {
                        object: expr.boxed(),
                        safe,
                        member,
                    };
                    (kind, member.span())
                }
                TokT::Bang => {
                    let bang = self.advance().span;
                    let kind = ExprKind::ForceUnwrap {
                        operand: expr.boxed(),
                        bang,
                    };
                    (kind, bang)
                }
                _ => break,
            };
            expr = Expr::new(kind, start.merge(end));
        }
        Ok(expr)
    }

    /// `[index]`, starting at the `[`. Returns the kind and the span of the `]`.
    fn index(&mut self, object: Expr<'src>, safe: Option<Span>) -> PResult<(ExprKind<'src>, Span)> {
        let open = self.advance_kind(TokT::LeftBracket).span;
        let index = self.expression()?;
        let close = self.close_delimiter(open, Delim::Bracket);
        let kind = ExprKind::Index {
            object: object.boxed(),
            safe,
            index: index.boxed(),
        };
        Ok((kind, close))
    }

    fn call_arg(&mut self) -> PResult<CallArg<'src>> {
        let label = if self.check(TokT::Identifier) && self.check_next(TokT::Colon) {
            let label = self.advance_ident();
            self.advance_kind(TokT::Colon);
            Some(label)
        } else {
            None
        };
        let value = self.expression()?;
        Ok(CallArg { label, value })
    }

    /// A tuple index. Anything but decimal digits and `_` (`t.0x1`, `t.1e2`) is an invalid number and gives
    /// `None`. A leading zero (`t.01`) and underscores are reported but the access is still built, since the
    /// intended index is clear. An index beyond `usize` is reported and approximated.
    fn tuple_index(&mut self) -> Option<Spanned<usize>> {
        let idx = self.advance_kind(TokT::Number);
        if !idx.lexeme.chars().all(|c| c.is_ascii_digit() || c == '_') {
            self.report(
                ParseErrorKind::InvalidNumber(NumberError::InvalidCharacter),
                idx.span,
            );
            return None;
        }
        let node = match strip_underscores(idx.lexeme).parse::<usize>() {
            Ok(index) => {
                if idx.lexeme != index.to_string() {
                    self.report(ParseErrorKind::TupleIndexNotPlain { index }, idx.span);
                }
                index
            }
            // The shape is checked above, but not the size.
            Err(_) => {
                self.report(ParseErrorKind::TupleIndexTooLarge, idx.span);
                usize::MAX
            }
        };
        Some(Spanned {
            node,
            span: idx.span,
        })
    }

    fn primary(&mut self) -> PResult<Expr<'src>> {
        match self.current.kind {
            TokT::True => Ok(self.literal(Literal::Boolean(true))),
            TokT::False => Ok(self.literal(Literal::Boolean(false))),
            TokT::Nil => Ok(self.literal(Literal::Nil)),
            TokT::Number => {
                let token = self.advance();

                let kind = match parse_number(token.lexeme) {
                    Ok(num) => ExprKind::Literal(Literal::Number(num)),
                    Err(error) => {
                        self.report(ParseErrorKind::InvalidNumber(error), token.span);
                        ExprKind::Error
                    }
                };
                Ok(Expr::new(kind, token.span))
            }
            TokT::String => {
                let token = self.advance();
                // Strip surrounding quotes.
                let text = self.escaped(token, 1..token.lexeme.len() - 1);
                Ok(Expr::new(
                    ExprKind::Literal(Literal::String(text)),
                    token.span,
                ))
            }
            TokT::RawString => {
                let token = self.advance();
                Ok(Expr::new(
                    ExprKind::Literal(Literal::String(process_raw_string(token.lexeme))),
                    token.span,
                ))
            }
            TokT::PartialString => self.string_interp(),
            TokT::Identifier | TokT::Self_ => {
                let token = self.advance();
                let name = Ident {
                    name: token.lexeme,
                    span: token.span,
                };
                Ok(Expr::new(ExprKind::Variable(name), token.span))
            }
            TokT::LeftBracket => Ok(self.list_or_map()),
            TokT::LeftParen => Ok(self.grouping_or_tuple()),
            TokT::LeftBrace => self.block_expr(),
            TokT::If => self.if_expr(),
            TokT::Match => self.match_expr(),
            TokT::Pipe => self.lambda(),
            TokT::Return => self.return_expr(),
            _ => Err(self.err_unexpected(Expected::Expression, None)),
        }
    }

    /// A one-token literal at the current token.
    fn literal(&mut self, literal: Literal) -> Expr<'src> {
        Expr::new(ExprKind::Literal(literal), self.advance().span)
    }

    /// The text of `token` in `range` (byte offsets into its lexeme) with escapes processed. A bad escape is
    /// reported on the escape sequence and leaves the raw text.
    fn escaped(&mut self, token: Token<'src>, range: Range<usize>) -> String {
        let raw = &token.lexeme[range.clone()];
        process_escapes(raw).unwrap_or_else(|(error, at)| {
            let span = token
                .span
                .sub_span(range.start + at.start..range.start + at.end);

            self.report(ParseErrorKind::InvalidEscape(error), span);
            raw.to_string()
        })
    }

    /// `(e)` is a grouping, `(e,)` and `(a, b)` are tuples, and `()` is an error.
    fn grouping_or_tuple(&mut self) -> Expr<'src> {
        let mut list = self.delimited(ListSpec::paren("expression", EXPR_START), Self::expression);
        let span = list.span();
        let kind = match (list.items.len(), list.trailing) {
            (0, _) => {
                if !list.failed {
                    self.report(ParseErrorKind::EmptyParens(ParenContext::Expression), span);
                }
                ExprKind::Error
            }
            (1, None) => ExprKind::Grouping(list.items.pop().expect("one item").boxed()),
            _ => ExprKind::Tuple(list.items),
        };
        Expr::new(kind, span)
    }

    /// `[a, b]`, `[k: v, …]`, `[]` or `[:]`. All items must agree on being plain or `key: value`, following
    /// the first one.
    fn list_or_map(&mut self) -> Expr<'src> {
        let open = self.advance_kind(TokT::LeftBracket).span;
        if self.check(TokT::Colon) && self.check_next(TokT::RightBracket) {
            self.advance();
            self.advance();
            return Expr::new(ExprKind::Map(vec![]), self.span_from(open));
        }

        let items =
            self.delimited_after(open, ListSpec::bracket("list element", EXPR_START), |p| {
                let key = p.expression()?;
                let value = if p.try_consume(TokT::Colon) {
                    Some(p.expression()?)
                } else {
                    None
                };
                Ok((key, value))
            });
        let span = items.span();

        let (first, is_map) = match items.items.first() {
            Some((key, Some(value))) => (key.span.merge(value.span), true),
            Some((key, None)) => (key.span, false),
            None => (open, false),
        };
        let mut entries = vec![];
        let mut elements = vec![];
        for (key, value) in items.items {
            match (is_map, value) {
                (true, Some(value)) => entries.push(MapEntry { key, value }),
                (false, None) => elements.push(key),
                (true, None) => {
                    self.report(ParseErrorKind::MixedCollection { first, is_map }, key.span)
                }
                (false, Some(value)) => self.report(
                    ParseErrorKind::MixedCollection { first, is_map },
                    key.span.merge(value.span),
                ),
            }
        }
        let kind = if is_map {
            ExprKind::Map(entries)
        } else {
            ExprKind::List(elements)
        };
        Expr::new(kind, span)
    }

    fn lambda(&mut self) -> PResult<Expr<'src>> {
        let start = self.start_span();
        let params = self.delimited(ListSpec::pipe("parameter", &[TokT::Identifier]), |p| {
            let name = p.expect_ident("`|`")?;
            let ty = p.type_annotation()?;
            Ok(LambdaParam { name, ty })
        });
        let ret = self.type_annotation()?.map(Box::new);
        let body = self.expression()?;
        Ok(Expr::new(
            ExprKind::Lambda {
                params: params.into_ast(),
                ret,
                body: body.boxed(),
            },
            self.span_from(start),
        ))
    }

    /// From a `PartialString` through the `String` that closes it, collecting literal parts and `${expr}`
    /// segments. Empty literal parts are left out.
    fn string_interp(&mut self) -> PResult<Expr<'src>> {
        let start = self.start_span();
        let first = self.advance_kind(TokT::PartialString);

        let mut parts: Vec<StringPart<'src>> = Vec::new();
        // PartialString lexeme: `"<content>` (opening quote, then raw content up to `${`).
        self.string_part(&mut parts, first, 1..first.lexeme.len());
        loop {
            parts.push(StringPart::Expr(self.expression()?));

            if self.check(TokT::String) {
                // String lexeme: `<content>"`.
                let end = self.advance();
                self.string_part(&mut parts, end, 0..end.lexeme.len() - 1);
                return Ok(Expr::new(
                    ExprKind::StringInterp(parts),
                    self.span_from(start),
                ));
            } else if self.check(TokT::PartialString) {
                let mid = self.advance();
                self.string_part(&mut parts, mid, 0..mid.lexeme.len());
            } else {
                return Err(self.err_unexpected(
                    Expected::Token(TokT::RightBrace),
                    Some("interpolated expression"),
                ));
            }
        }
    }

    /// Pushes the text of `token` in `range` as a literal part, unless it is empty.
    fn string_part(
        &mut self,
        parts: &mut Vec<StringPart<'src>>,
        token: Token<'src>,
        range: Range<usize>,
    ) {
        let value = self.escaped(token, range.clone());
        if !value.is_empty() {
            parts.push(StringPart::Literal {
                value,
                span: token.span.sub_span(range),
            });
        }
    }
}

fn binary<'src>(left: Expr<'src>, op: Spanned<BinaryOp>, right: Expr<'src>) -> Expr<'src> {
    let span = left.span.merge(right.span);
    Expr::new(
        ExprKind::Binary {
            left: left.boxed(),
            op,
            right: right.boxed(),
        },
        span,
    )
}
