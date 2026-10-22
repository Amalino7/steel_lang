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

use crate::parser::error::ParserError;
use crate::scanner::TokenType as TokT;
use crate::scanner::{Scanner, Span, Token, TokenType};
use ast::ParsedFile;
use std::cell::Cell;
use std::mem;

pub(crate) type PResult<'src, T> = Result<T, ParserError<'src>>;

/// `check`s allowed without an `advance` in between before the parser is assumed to loop forever.
const FUEL: u32 = 100_000;

pub struct Parser<'src> {
    scanner: Scanner<'src>,
    previous: Token<'src>,
    current: Token<'src>,
    next: Token<'src>,
    errors: Vec<ParserError<'src>>,
    /// Tokens consumed so far. Recovery points compare it before and after an item to guarantee progress.
    pos: u32,
    /// Start of the first unterminated string or comment. It runs to EOF, so every parse error after it is noise from open constructs.
    unterminated_at: Option<usize>,
    /// Debug-only loop detector: `check`s since the last `advance`.
    steps: Cell<u32>,
}

impl<'src> Parser<'src> {
    pub fn new(scanner: Scanner<'src>) -> Self {
        let placeholder = Token::new(TokT::EOF, Span::default(), "");
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

    pub fn parse(&mut self) -> Result<ParsedFile<'src>, Vec<ParserError<'src>>> {
        let imports = self.parse_imports();
        let body = self.sequence(&[TokT::EOF], Self::recover_top_level, |p| {
            p.top_level_item()
        });
        let errors = self.take_errors();
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(ParsedFile { imports, body })
        }
    }

    /// The errors so far, in source order, without the noise after an unterminated string or comment, and with
    /// one error per position: a second error where one was already reported is a cascade (an unclosed `(` at
    /// the end of a statement is also a missing `;`).
    fn take_errors(&mut self) -> Vec<ParserError<'src>> {
        let mut errors = mem::take(&mut self.errors);
        if let Some(at) = self.unterminated_at {
            errors.retain(|e| matches!(e, ParserError::ScannerError { .. }) || e.span().start < at);
        }
        errors.sort_by_key(|e| e.span().start);
        errors.dedup_by_key(|e| e.span().start);
        errors
    }

    /// The only caller that works with the scanner directly.
    /// Reports lexer errors so they are ignored by the grammar rules.
    fn scan_next(&mut self) -> Token<'src> {
        loop {
            let token = self.scanner.next_token();
            match token.kind {
                TokT::UnexpectedSymbolError => self.errors.push(ParserError::UnexpectedToken {
                    found: token,
                    message: "Invalid token".to_string(),
                }),
                // Every `Error` token is an unterminated string or comment.
                TokT::Error => {
                    self.unterminated_at.get_or_insert(token.span.start);
                    self.errors.push(ParserError::ScannerError { token });
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
        self.steps.set(0);
        self.pos += 1;
        self.previous = self.current;
        self.current = self.next;
        self.next = self.scan_next();
        self.previous
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
        self.check(TokT::EOF)
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

    /// Consumes a required `kind`. The `after` argument names what precedes it.
    /// Used for "missing `kind` after <after>" errors.
    fn expect(&mut self, kind: TokenType, after: &'static str) -> PResult<'src, Token<'src>> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            Err(self.err_expected(kind, after))
        }
    }

    fn expect_ident(&mut self, after: &'static str) -> PResult<'src, Token<'src>> {
        if self.check(TokT::Identifier) {
            Ok(self.advance())
        } else if self.current.kind.is_keyword() {
            Err(ParserError::ParseError {
                token: self.current,
                message: format!(
                    "`{}` is a keyword and can't be used as a name.",
                    self.current.lexeme
                ),
            })
        } else {
            Err(self.err_expected(TokT::Identifier, after))
        }
    }

    /// Consumes a `;`. If it's missing but the statement clearly ended (the current token is `}`, EOF, or the first
    /// on a new line), reports the error and returns `Ok` as if it were there, so the next statement still
    /// parses. Otherwise, it returns `Err`.
    fn expect_semi(&mut self, after: &'static str) -> PResult<'src, ()> {
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

    /// Expected `expected` at the current position.
    /// When the current token starts a new line, the error points just
    /// after the previous token instead.
    fn err_expected(&self, expected: TokenType, after: &'static str) -> ParserError<'src> {
        ParserError::MissingToken {
            expected,
            after,
            found: self.current,
            span: self.expected_span(),
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
        start.to(self.previous.span)
    }
}

#[cfg(test)]
mod tests;
