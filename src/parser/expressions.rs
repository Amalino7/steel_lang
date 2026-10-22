//! Expressions: precedence climbing from assignment down to `primary`, postfix chains (calls, indexing, field
//! access, turbofish), literals, collections, lambdas and string interpolation.

use crate::parser::ast::{CallArg, Expr, Literal, StringPart, TypeAst};
use crate::parser::error::ParserError;
use crate::parser::lists::{Delim, ListSpec};
use crate::parser::literals::{parse_number, process_escapes, process_raw_string};
use crate::parser::{PResult, Parser, TokT};
use crate::scanner::token::Token;

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

/// The binary operator a compound assignment applies (`+=` → `+`), or `None` if `kind` isn't one.
fn compound_operator(kind: TokT) -> Option<TokT> {
    match kind {
        TokT::PlusEqual => Some(TokT::Plus),
        TokT::MinusEqual => Some(TokT::Minus),
        TokT::StarEqual => Some(TokT::Star),
        TokT::SlashEqual => Some(TokT::Slash),
        TokT::PercentEqual => Some(TokT::Percent),
        _ => None,
    }
}

impl<'src> Parser<'src> {
    pub(crate) fn expression(&mut self) -> PResult<'src, Expr<'src>> {
        self.assignment()
    }

    fn assignment(&mut self) -> PResult<'src, Expr<'src>> {
        let expr = self.null_coalescing()?;

        if self.try_consume(TokT::Equal) {
            let value = self.assignment()?;
            return Ok(self.assignment_target(expr, value));
        }
        if let Some(kind) = compound_operator(self.current.kind) {
            let operator = Token {
                kind,
                ..self.advance()
            };
            let right = self.assignment()?;
            let binary = Expr::Binary {
                left: Box::new(expr.clone()),
                operator,
                right: Box::new(right),
            };
            return Ok(self.assignment_target(expr, binary));
        }
        Ok(expr)
    }

    /// Turns the parsed left-hand side into an assignment. An invalid target is reported and parsing goes on.
    fn assignment_target(&mut self, target: Expr<'src>, value: Expr<'src>) -> Expr<'src> {
        let value = Box::new(value);
        match target {
            Expr::Variable { name } => Expr::Assignment {
                identifier: name,
                value,
            },
            Expr::Get {
                object,
                field,
                safe,
            } => Expr::Set {
                safe,
                object,
                field,
                value,
            },
            Expr::GetIndex {
                safe,
                object,
                index,
            } => Expr::SetIndex {
                safe,
                object,
                index,
                value,
            },
            other => {
                self.errors
                    .push(ParserError::InvalidAssignmentTarget { span: other.span() });
                Expr::Error(other.span().to(value.span()))
            }
        }
    }

    fn null_coalescing(&mut self) -> PResult<'src, Expr<'src>> {
        let mut expr = self.logical_or()?;
        while self.try_consume(TokT::QuestionQuestion) {
            let operator = self.previous;
            let right = self.logical_or()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn logical_or(&mut self) -> PResult<'src, Expr<'src>> {
        let mut expr = self.logical_and()?;
        while self.try_consume(TokT::Or) {
            let operator = self.previous;
            let right = self.logical_and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn logical_and(&mut self) -> PResult<'src, Expr<'src>> {
        let mut expr = self.equality()?;
        while self.try_consume(TokT::And) {
            let operator = self.previous;
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    /// One left-associative binary precedence level: `next (op next)*` for `op` in `ops`.
    fn binary_level(
        &mut self,
        ops: &[TokT],
        next: fn(&mut Self) -> PResult<'src, Expr<'src>>,
    ) -> PResult<'src, Expr<'src>> {
        let mut expr = next(self)?;
        while self.try_consume_any(ops) {
            let operator = self.previous;
            let right = next(self)?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn equality(&mut self) -> PResult<'src, Expr<'src>> {
        self.binary_level(&[TokT::EqualEqual, TokT::BangEqual], Self::comparison)
    }

    fn comparison(&mut self) -> PResult<'src, Expr<'src>> {
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

    fn term(&mut self) -> PResult<'src, Expr<'src>> {
        self.binary_level(&[TokT::Minus, TokT::Plus], Self::factor)
    }

    fn factor(&mut self) -> PResult<'src, Expr<'src>> {
        self.binary_level(&[TokT::Star, TokT::Slash, TokT::Percent], Self::unary)
    }

    fn unary(&mut self) -> PResult<'src, Expr<'src>> {
        if self.try_consume_any(&[TokT::Bang, TokT::Minus, TokT::Try]) {
            let operator = self.previous;
            let expression = self.unary()?;
            Ok(Expr::Unary {
                operator,
                expression: Box::new(expression),
            })
        } else {
            self.power()
        }
    }

    fn power(&mut self) -> PResult<'src, Expr<'src>> {
        let base = self.is_type()?;
        if self.try_consume(TokT::StarStar) {
            let operator = self.previous;
            let exponent = self.unary()?;
            Ok(Expr::Binary {
                left: Box::new(base),
                operator,
                right: Box::new(exponent),
            })
        } else {
            Ok(base)
        }
    }

    fn is_type(&mut self) -> PResult<'src, Expr<'src>> {
        let expr = self.call()?;
        if self.try_consume(TokT::Is) {
            let type_name = self.expect_ident("'is'")?;
            return Ok(Expr::Is {
                expression: Box::new(expr),
                type_name,
            });
        }
        Ok(expr)
    }

    /// `primary` followed by any chain of calls, indexing, field/tuple access, turbofish and `!`.
    fn call(&mut self) -> PResult<'src, Expr<'src>> {
        let mut expr = self.primary()?;
        loop {
            expr = match self.current.kind {
                TokT::LeftParen | TokT::QuestionParen => {
                    let open = self.advance();
                    let args = self.delimited_after(
                        open.span,
                        ListSpec::paren("argument", EXPR_START),
                        Self::call_arg,
                    );
                    Expr::Call {
                        callee: Box::new(expr),
                        arguments: args.items,
                        safe: open.kind == TokT::QuestionParen,
                    }
                }
                TokT::Dot if self.check_next(TokT::Less) => {
                    self.advance();
                    Expr::TypeSpecialization {
                        callee: Box::new(expr),
                        generics: self.generic_arg_list(),
                    }
                }
                TokT::Question if self.check_next(TokT::LeftBracket) => {
                    self.advance();
                    self.index(expr, true)?
                }
                TokT::LeftBracket => self.index(expr, false)?,
                TokT::Dot | TokT::QuestionDot => {
                    let safe = self.advance().kind == TokT::QuestionDot;
                    let field = if self.try_consume(TokT::Number) {
                        self.tuple_index()
                    } else {
                        self.expect_ident(if safe { "'?.'" } else { "'.'" })?
                    };
                    Expr::Get {
                        safe,
                        object: Box::new(expr),
                        field,
                    }
                }
                TokT::Bang => Expr::ForceUnwrap {
                    expression: Box::new(expr),
                    operator: self.advance(),
                },
                _ => break,
            };
        }
        Ok(expr)
    }

    /// `[index]`, starting at the `[`.
    fn index(&mut self, object: Expr<'src>, safe: bool) -> PResult<'src, Expr<'src>> {
        let open = self.advance_kind(TokT::LeftBracket).span;
        let index = self.expression()?;
        self.close_delimiter(open, Delim::Bracket);
        Ok(Expr::GetIndex {
            safe,
            object: Box::new(object),
            index: Box::new(index),
        })
    }

    fn call_arg(&mut self) -> PResult<'src, CallArg<'src>> {
        let label = if self.check(TokT::Identifier) && self.check_next(TokT::Colon) {
            let label = self.advance();
            self.advance_kind(TokT::Colon);
            Some(label)
        } else {
            None
        };
        let expr = self.expression()?;
        Ok(CallArg { label, expr })
    }

    /// The tuple index just consumed. The scanner guarantees it is decimal digits; a leading zero (`t.01`) is
    /// reported but the access is still built, since the intended index is clear.
    fn tuple_index(&mut self) -> Token<'src> {
        let idx = self.previous;
        if idx.lexeme.len() > 1 && idx.lexeme.starts_with('0') {
            self.errors.push(ParserError::ParseError {
                token: idx,
                message: "Tuple index must not have leading zeros.".to_string(),
            });
        }
        idx
    }

    fn primary(&mut self) -> PResult<'src, Expr<'src>> {
        match self.current.kind {
            TokT::True => Ok(self.literal(Literal::Boolean(true))),
            TokT::False => Ok(self.literal(Literal::Boolean(false))),
            TokT::Nil => Ok(self.literal(Literal::Nil)),
            TokT::Number => {
                let token = self.advance();
                match parse_number(token.lexeme) {
                    Some(num) => Ok(Expr::Literal {
                        literal: Literal::Number(num),
                        span: token.span,
                    }),
                    None => {
                        self.errors.push(ParserError::ParseError {
                            token,
                            message: "Invalid number literal.".to_string(),
                        });
                        Ok(Expr::Error(token.span))
                    }
                }
            }
            TokT::String => {
                let token = self.advance();
                // Strip surrounding quotes.
                let inner = &token.lexeme[1..token.lexeme.len() - 1];
                let text = self.escaped(token, inner);
                Ok(Expr::Literal {
                    literal: Literal::String(text),
                    span: token.span,
                })
            }
            TokT::RawString => {
                let token = self.advance();
                Ok(Expr::Literal {
                    literal: Literal::String(process_raw_string(token.lexeme)),
                    span: token.span,
                })
            }
            TokT::PartialString => self.string_interp(),
            TokT::Identifier | TokT::Self_ => Ok(Expr::Variable {
                name: self.advance(),
            }),
            TokT::LeftBracket => Ok(self.list_or_map()),
            TokT::LeftParen => Ok(self.grouping_or_tuple()),
            TokT::LeftBrace => self.block_expr(),
            TokT::If => self.if_expr(),
            TokT::Match => self.match_expr(),
            TokT::Pipe => self.lambda(),
            TokT::Return => self.return_expr(),
            _ => Err(ParserError::ParseError {
                token: self.current,
                message: format!("Expected expression, but found '{}'.", self.current.lexeme),
            }),
        }
    }

    /// A one-token literal at the current token.
    fn literal(&mut self, literal: Literal) -> Expr<'src> {
        Expr::Literal {
            literal,
            span: self.advance().span,
        }
    }

    /// `raw` with escapes processed. A bad escape is reported on `token` and leaves the raw text.
    fn escaped(&mut self, token: Token<'src>, raw: &str) -> String {
        process_escapes(raw).unwrap_or_else(|message| {
            self.errors.push(ParserError::ParseError { token, message });
            raw.to_string()
        })
    }

    /// `(e)` is a grouping, `(e,)` and `(a, b)` are tuples, and `()` is an error.
    fn grouping_or_tuple(&mut self) -> Expr<'src> {
        let mut list = self.delimited(ListSpec::paren("expression", EXPR_START), Self::expression);
        match (list.items.len(), list.trailing) {
            (0, _) => {
                if !list.failed {
                    self.errors.push(ParserError::Invalid {
                        span: list.open.to(list.close),
                        message: "`()` is not an expression.".to_string(),
                    });
                }
                Expr::Error(list.open.to(list.close))
            }
            (1, None) => Expr::Grouping {
                expression: Box::new(list.items.pop().expect("one item")),
            },
            _ => Expr::Tuple {
                elements: list.items,
            },
        }
    }

    /// `[a, b]`, `[k: v, …]`, `[]` or `[:]`. All items must agree on being plain or `key: value`, following
    /// the first one.
    fn list_or_map(&mut self) -> Expr<'src> {
        let bracket_token = self.advance_kind(TokT::LeftBracket);
        if self.check(TokT::Colon) && self.check_next(TokT::RightBracket) {
            self.advance();
            self.advance();
            return Expr::Map {
                kv_pairs: vec![],
                bracket_token,
            };
        }

        let items = self.delimited_after(
            bracket_token.span,
            ListSpec::bracket("list element", EXPR_START),
            |p| {
                let key = p.expression()?;
                let value = if p.try_consume(TokT::Colon) {
                    Some(p.expression()?)
                } else {
                    None
                };
                Ok((key, value))
            },
        );

        let is_map = items.items.first().is_some_and(|(_, v)| v.is_some());
        let mut kv_pairs = vec![];
        let mut elements = vec![];
        for (key, value) in items.items {
            match (is_map, value) {
                (true, Some(value)) => kv_pairs.push((key, value)),
                (false, None) => elements.push(key),
                (true, None) => self.errors.push(ParserError::Invalid {
                    span: key.span(),
                    message: "Expected `key: value` in map literal.".to_string(),
                }),
                (false, Some(value)) => self.errors.push(ParserError::Invalid {
                    span: key.span().to(value.span()),
                    message: "Unexpected `key: value` in list literal.".to_string(),
                }),
            }
        }
        if is_map {
            Expr::Map {
                kv_pairs,
                bracket_token,
            }
        } else {
            Expr::List {
                elements,
                bracket_token,
            }
        }
    }

    fn lambda(&mut self) -> PResult<'src, Expr<'src>> {
        let pipe_token = self.current;
        let params = self.delimited(ListSpec::pipe("parameter", &[TokT::Identifier]), |p| {
            let name = p.expect_ident("'|'")?;
            let ty = p.type_annotation()?.unwrap_or(TypeAst::Infer);
            Ok((name, ty))
        });
        let return_type = self.return_type()?;
        let body = self.expression()?;
        Ok(Expr::Lambda {
            params: params.items,
            return_type,
            body: Box::new(body),
            pipe_token,
        })
    }

    /// From a `PartialString` through the `String` that closes it, collecting literal parts and `${expr}`
    /// segments.
    fn string_interp(&mut self) -> PResult<'src, Expr<'src>> {
        let start = self.start_span();
        let first = self.advance_kind(TokT::PartialString);

        // PartialString lexeme: `"<content>` (opening quote, then raw content up to `${`).
        let first_text = self.escaped(first, &first.lexeme[1..]);
        let mut parts: Vec<StringPart<'src>> = Vec::new();
        if !first_text.is_empty() {
            parts.push(StringPart::Literal(first_text));
        }

        loop {
            let expr = self.expression()?;
            parts.push(StringPart::Expr(Box::new(expr)));

            if self.check(TokT::String) {
                // String lexeme: `<content>"`.
                let end = self.advance();
                let end_text = self.escaped(end, &end.lexeme[..end.lexeme.len() - 1]);
                if !end_text.is_empty() {
                    parts.push(StringPart::Literal(end_text));
                }
                return Ok(Expr::StringInterp {
                    parts,
                    span: self.span_from(start),
                });
            } else if self.check(TokT::PartialString) {
                let mid = self.advance();
                let mid_text = self.escaped(mid, mid.lexeme);
                if !mid_text.is_empty() {
                    parts.push(StringPart::Literal(mid_text));
                }
            } else {
                return Err(ParserError::ParseError {
                    token: self.current,
                    message: "Expected string interpolation to continue or close.".to_string(),
                });
            }
        }
    }
}
