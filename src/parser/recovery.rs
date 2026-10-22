//! Error recovery: the restart token sets, balanced skipping and the separator-less item loop (`sequence`).
//!
//! Callers are the recovery points (`sequence` for top level, blocks and `impl`/`interface` bodies,
//! `list_items`, the match-arm loop), plus `close_delimiter` and the import header. Everything else returns
//! `Err` and lets one of them recover. An error is always pushed before recovering.

use crate::parser::{PResult, Parser};
use crate::scanner::TokenType as TokT;

// Restart points. Slices don't union, so a set that extends another is written out in full and a test keeps the
// two in sync.

/// Tokens that always start a declaration. `Func` is not in the set because it also starts a function type; see
/// [`LINE_START_STOPS`].
const DECL_START: &[TokT] = &[
    TokT::Struct,
    TokT::Enum,
    TokT::Impl,
    TokT::Interface,
    TokT::Extern,
    TokT::Public,
    TokT::Import,
    TokT::Let,
];

/// Tokens that always start a statement: [`DECL_START`] plus `while`.
const STMT_START: &[TokT] = &[
    TokT::Struct,
    TokT::Enum,
    TokT::Impl,
    TokT::Interface,
    TokT::Extern,
    TokT::Public,
    TokT::Import,
    TokT::Let,
    TokT::While,
];

/// Restart points only when they are the first token on their line: mid-line they are usually part of the
/// expression being skipped (`let x = if …`, `let f: func(…)`).
const LINE_START_STOPS: &[TokT] = &[TokT::Func, TokT::If, TokT::Match, TokT::Return];

/// Where a list item's or match arm's recovery gives up on the enclosing construct: `;` and [`STMT_START`].
/// Keeps an unclosed `f(a, b⏎let x = 1;` from swallowing the rest of the file.
pub(super) const RECOVERY_STOP: &[TokT] = &[
    TokT::Semicolon,
    TokT::Struct,
    TokT::Enum,
    TokT::Impl,
    TokT::Interface,
    TokT::Extern,
    TokT::Public,
    TokT::Import,
    TokT::Let,
    TokT::While,
];

fn is_opener(k: TokT) -> bool {
    matches!(
        k,
        TokT::LeftParen | TokT::QuestionParen | TokT::LeftBracket | TokT::LeftBrace
    )
}

pub(super) fn is_closer(k: TokT) -> bool {
    matches!(k, TokT::RightParen | TokT::RightBracket | TokT::RightBrace)
}

impl<'src> Parser<'src> {
    /// Skips tokens until the current token is in one of `stops` at nesting depth 0, is an unmatched closer, or
    /// is EOF. Never consumes the stop token or the unmatched closer: the closer belongs to an enclosing
    /// construct, whose `close_delimiter` needs it.
    pub(super) fn skip_to(&mut self, stops: &[&[TokT]]) {
        self.skip_until(|p| stops.iter().any(|set| p.check_any(set)));
    }

    /// [`Self::skip_to`] with an arbitrary stop condition, evaluated at depth 0 only.
    ///
    /// Depth is one counter over `()`, `[]` and `{}`, so `(]` counts as balanced; that's enough for recovery.
    /// `|` opens and closes, so it isn't counted. String interpolation needs no special case: the scanner folds
    /// the `}` that ends `${…}` into the next string piece.
    fn skip_until(&mut self, stop: impl Fn(&Self) -> bool) {
        let mut depth = 0u32;
        while !self.at_eof() {
            let kind = self.current.kind;
            if depth == 0 && (is_closer(kind) || stop(self)) {
                return;
            }
            if is_opener(kind) {
                depth += 1;
            } else if is_closer(kind) {
                depth -= 1;
            }
            self.advance();
        }
    }

    /// Statement recovery inside a block: skips past the next `;`, or to the next statement start, an unmatched
    /// closer or EOF. `func`/`if`/`match`/`return` restart only at the start of a line.
    pub(super) fn recover_stmt(&mut self) {
        self.skip_until(|p| {
            p.check(TokT::Semicolon)
                || p.check_any(STMT_START)
                || (p.check_any(LINE_START_STOPS) && p.at_line_start())
        });
        self.try_consume(TokT::Semicolon);
    }

    /// Top-level recovery: like [`Self::recover_stmt`] (the top level holds statements too), but an unmatched
    /// closer has no enclosing construct here, so it is stepped over.
    pub(super) fn recover_top_level(&mut self) {
        loop {
            self.recover_stmt();
            if !is_closer(self.current.kind) || self.previous.kind == TokT::Semicolon {
                return;
            }
            self.advance();
        }
    }

    /// Item recovery inside an `impl` or `interface` body: skips to the next declaration keyword, a line-start
    /// `func`, past a `;`, or to the unmatched `}` that ends the body.
    pub(super) fn recover_item(&mut self) {
        self.skip_until(|p| {
            p.check(TokT::Semicolon)
                || p.check_any(DECL_START)
                || (p.check(TokT::Func) && p.at_line_start())
        });
        self.try_consume(TokT::Semicolon);
    }

    /// Parses `item*` until the current token is in `close` (not consumed) or EOF. A failed item is reported and
    /// followed by `recover`. If recovery didn't move, the current token is one no item accepts but `recover`
    /// stops on (a stray `)`, or `let` in an `impl` body): it is stepped over and recovery runs again, so the
    /// loop always progresses and the rest of the bad item isn't reported a second time.
    pub(super) fn sequence<T>(
        &mut self,
        close: &[TokT],
        recover: fn(&mut Self),
        mut item: impl FnMut(&mut Self) -> PResult<'src, T>,
    ) -> Vec<T> {
        let mut items = vec![];
        while !self.check_any(close) && !self.at_eof() {
            let before = self.pos;
            match item(self) {
                Ok(t) => items.push(t),
                Err(e) => {
                    self.errors.push(e);
                    recover(self);
                    if self.pos == before && !self.check_any(close) {
                        self.advance();
                        recover(self);
                    }
                }
            }
        }
        items
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn is_subset(sub: &[TokT], sup: &[TokT]) -> bool {
        sub.iter().all(|k| sup.contains(k))
    }

    #[test]
    fn test_stmt_start_extends_decl_start() {
        assert!(is_subset(DECL_START, STMT_START));
        assert_eq!(STMT_START.len(), DECL_START.len() + 1);
        assert!(STMT_START.contains(&TokT::While));
    }

    #[test]
    fn test_recovery_stop_is_semicolon_and_stmt_start() {
        assert!(is_subset(STMT_START, RECOVERY_STOP));
        assert_eq!(RECOVERY_STOP.len(), STMT_START.len() + 1);
        assert!(RECOVERY_STOP.contains(&TokT::Semicolon));
    }
}
