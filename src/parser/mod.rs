//! Recursive-descent parser: tokens → untyped AST (`ast.rs`).
//!
//! Runs after the scanner, per file, and feeds the resolver (imports) and the typechecker (body). This file
//! owns the token cursor; `lists.rs` and `recovery.rs` hold the shared list and recovery helpers, and the
//! remaining files parse one area of the grammar each.
//!
//! Conventions every sub-parser follows (see `docs/features/parser-improvements/phase2-infrastructure.md`):
//! - A parser is called with `current` on its own first token and consumes it itself (`advance_kind` when the
//!   caller dispatched on it). Dispatchers `match` on `current` without consuming.
//! - A function that can recover is infallible; one that can't returns `Err` to the nearest recovery point
//!   (`sequence`, `list_items`, the match-arm loop). Only those call `skip_to`/`recover_*`.
//! - Lexer error tokens never reach the grammar: `scan_next` reports and drops them.

pub mod ast;
mod control_flow;
mod declarations;
pub mod error;
mod expressions;
mod lists;
mod literals;
pub mod printer;
mod recovery;
mod statements;

use crate::parser::error::{Expected, Found, ParseError, ParseErrorKind};
use crate::scanner::TokenType as TokT;
use crate::scanner::{Scanner, Span, Token, TokenType};
use ast::{Ident, ParsedFile};
use std::cell::Cell;
use std::mem;

pub(crate) type PResult<T> = Result<T, ParseError>;

/// `check`s allowed without an `advance` in between before the parser is assumed to loop forever.
const FUEL: u32 = 100_000;

pub struct Parser<'src> {
    scanner: Scanner<'src>,
    previous: Token<'src>,
    current: Token<'src>,
    next: Token<'src>,
    errors: Vec<ParseError>,
    /// Tokens consumed so far. Recovery points compare it before and after an item to guarantee progress.
    pos: u32,
    /// Start of the first unterminated string or comment. It runs to EOF, so every parse error after it is noise from open constructs.
    unterminated_at: Option<usize>,
    /// Debug-only loop detector: `check`s since the last `advance`.
    steps: Cell<u32>,
}

impl<'src> Parser<'src> {
    pub fn new(scanner: Scanner<'src>) -> Self {
        let placeholder = Token::new(TokT::EoF, Span::default(), "");
        let mut parser = Parser {
            scanner,
            previous: placeholder,
            current: placeholder,
            next: placeholder,
            errors: Vec::new(),
            pos: 0,
            unterminated_at: None,
            steps: Cell::new(0),
        };
        parser.current = parser.scan_next();
        parser.previous = parser.current;
        parser.next = parser.scan_next();
        parser
    }

    pub fn parse(&mut self) -> Result<ParsedFile<'src>, Vec<ParseError>> {
        let imports = self.parse_imports();
        let stmts = self.sequence(&[TokT::EoF], Self::recover_top_level, |p| {
            p.top_level_item()
        });
        let errors = self.take_errors();
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(ParsedFile { imports, stmts })
        }
    }

    /// The errors so far, in source order, without the noise after an unterminated string or comment, and with
    /// at most one missing terminator per position (see [`terminator_rank`]).
    fn take_errors(&mut self) -> Vec<ParseError> {
        let mut errors = mem::take(&mut self.errors);
        if let Some(at) = self.unterminated_at {
            errors
                .retain(|e| matches!(e.kind, ParseErrorKind::Unterminated(_)) || e.span.start < at);
        }
        errors.sort_by_key(|e| (e.span.start, terminator_rank(e)));
        errors.dedup_by(|e, kept| {
            e.span.start == kept.span.start
                && terminator_rank(e).is_some()
                && terminator_rank(kept).is_some()
        });
        errors
    }

    /// The only caller that works with the scanner directly.
    /// Reports lexer errors so they are ignored by the grammar rules.
    fn scan_next(&mut self) -> Token<'src> {
        loop {
            let token = self.scanner.next_token();
            match token.kind {
                TokT::UnexpectedSymbolError => self.errors.push(ParseError::new(
                    ParseErrorKind::InvalidCharacter,
                    token.span,
                )),
                TokT::Unterminated(what) => {
                    self.unterminated_at.get_or_insert(token.span.start);
                    self.errors.push(ParseError::new(
                        ParseErrorKind::Unterminated(what),
                        token.span,
                    ));
                }
                _ => return token,
            }
        }
    }

    /// Consumes the current token and returns it.
    /// At EOF it avoids burning fuel.
    fn advance(&mut self) -> Token<'src> {
        if self.at_eof() {
            return self.current;
        }
        let next = self.scan_next();
        let current = mem::replace(&mut self.next, next);
        self.shift(self.current, current)
    }

    /// Consumes the first character of the current token as a `first` token and leaves the rest as the current
    /// token, of kind `rest`. For a closer the scanner glued to what follows it (see `lists::glued_rest`).
    fn split_current(&mut self, first: TokenType, rest: TokenType) -> Token<'src> {
        let token = self.current;
        let mid = token.span.start + 1;
        let head = Token::new(
            first,
            Span {
                end: mid,
                ..token.span
            },
            &token.lexeme[..1],
        );
        let tail = Token::new(
            rest,
            Span {
                start: mid,
                ..token.span
            },
            &token.lexeme[1..],
        );
        self.shift(head, tail)
    }

    /// The one place the cursor moves: `consumed` becomes `previous` and `current` the current token.
    fn shift(&mut self, consumed: Token<'src>, current: Token<'src>) -> Token<'src> {
        self.steps.set(0);
        self.pos += 1;
        self.previous = consumed;
        self.current = current;
        consumed
    }

    /// Advances over a token the caller has already checked. A different kind is a parser bug.
    fn advance_kind(&mut self, k: TokenType) -> Token<'src> {
        debug_assert_eq!(
            self.current.kind, k,
            "parser dispatched on the wrong token: {:?}",
            self.current
        );
        self.advance()
    }

    fn burn_fuel(&self) {
        if cfg!(debug_assertions) {
            let steps = self.steps.get() + 1;
            assert!(steps < FUEL, "parser is stuck at {:?}", self.current);
            self.steps.set(steps);
        }
    }

    #[must_use]
    fn check(&self, k: TokenType) -> bool {
        self.burn_fuel();
        self.current.kind == k
    }

    #[must_use]
    fn check_any(&self, ks: &[TokenType]) -> bool {
        self.burn_fuel();
        ks.contains(&self.current.kind)
    }

    #[must_use]
    fn check_next(&self, k: TokenType) -> bool {
        self.next.kind == k
    }

    #[must_use]
    fn at_eof(&self) -> bool {
        self.check(TokT::EoF)
    }

    /// The current token is the first one on its line.
    #[must_use]
    fn at_line_start(&self) -> bool {
        self.current.span.line > self.previous.span.line
    }

    /// Consumes the current token if it is `kind`.
    fn try_consume(&mut self, kind: TokenType) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Consumes the current token if it is in `kinds`.
    fn try_consume_any(&mut self, kinds: &[TokenType]) -> bool {
        if self.check_any(kinds) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Reports a specific parser error `kind` at `span`
    fn report(&mut self, kind: ParseErrorKind, span: Span) {
        self.errors.push(ParseError::new(kind, span));
    }

    /// Consumes a required `kind`. The `after` argument names what precedes it.
    /// Used for "missing `kind` after <after>" errors.
    fn expect(&mut self, kind: TokenType, after: &'static str) -> PResult<Token<'src>> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(self.err_expected(kind, after))
        }
    }

    fn expect_ident(&mut self, after: &'static str) -> PResult<Ident<'src>> {
        if self.check(TokT::Identifier) {
            Ok(self.advance_ident())
        } else {
            Err(self.err_name(after))
        }
    }

    /// Advances over an identifier the caller has already checked.
    fn advance_ident(&mut self) -> Ident<'src> {
        let token = self.advance_kind(TokT::Identifier);
        Ident {
            name: token.lexeme,
            span: token.span,
        }
    }

    /// A name is missing after `after`. A keyword where the name belongs gets its own error.
    fn err_name(&self, after: &'static str) -> ParseError {
        if self.current.kind.is_keyword() {
            ParseError::new(
                ParseErrorKind::KeywordAsName {
                    keyword: self.current.kind,
                },
                self.current.span,
            )
        } else {
            self.err_expected(TokT::Identifier, after)
        }
    }

    /// Consumes a `;`. If it's missing but the statement clearly ended (the current token is `}`, EOF, or the first
    /// on a new line), reports the error and returns `Ok` as if it were there, so the next statement still
    /// parses. Otherwise, it returns `Err`.
    fn expect_semi(&mut self, after: &'static str) -> PResult<()> {
        if self.try_consume(TokT::Semicolon) {
            return Ok(());
        }
        let err = self.err_expected(TokT::Semicolon, after);
        if self.check(TokT::RightBrace) || self.at_eof() || self.at_line_start() {
            self.errors.push(err);
            Ok(())
        } else {
            Err(err)
        }
    }

    /// The token `expected` (a name for `Identifier`) is missing after `after`. Reported where it belongs: at the
    /// current token, or just after the previous token when the current one starts a new line.
    fn err_expected(&self, expected: TokenType, after: &'static str) -> ParseError {
        let expected = match expected {
            TokT::Identifier => Expected::Name,
            kind => Expected::Token(kind),
        };
        let kind = ParseErrorKind::Expected {
            expected,
            after: Some(after),
            found: self.found(),
        };
        ParseError::new(kind, self.expected_span())
    }

    /// The current token can't start `expected`. Reported on the current token, which is the problem itself.
    fn err_unexpected(&self, expected: Expected, after: Option<&'static str>) -> ParseError {
        let kind = ParseErrorKind::Expected {
            expected,
            after,
            found: self.found(),
        };
        ParseError::new(kind, self.current.span)
    }

    fn found(&self) -> Found {
        Found {
            kind: self.current.kind,
            span: self.current.span,
        }
    }

    fn expected_span(&self) -> Span {
        if self.at_line_start() {
            self.previous.span.point_after()
        } else {
            self.current.span
        }
    }

    fn start_span(&self) -> Span {
        self.current.span
    }

    /// From `start` to the end of the last consumed token. Something must have been consumed since `start`.
    fn span_from(&self, start: Span) -> Span {
        debug_assert!(
            start.start <= self.previous.span.start,
            "span_from with nothing consumed since `start`"
        );
        start.merge(self.previous.span)
    }
}

/// Where a missing terminator ranks among the errors at its position, best first; `None` for any other error.
///
/// A recovery that reports a missing terminator doesn't consume, so every enclosing construct reports its own
/// at the same point, and only the best one is kept. An unclosed delimiter explains the rest: in `let v = { 1`,
/// the `;` after `1` and after the `let` are only missing because the `}` is.
fn terminator_rank(error: &ParseError) -> Option<u8> {
    match error.kind {
        ParseErrorKind::UnclosedDelimiter { .. } => Some(0),
        ParseErrorKind::Expected {
            expected: Expected::Token(_) | Expected::Name,
            ..
        }
        | ParseErrorKind::MissingSeparator { .. } => Some(1),
        _ => None,
    }
}

#[cfg(test)]
mod tests;
