//! Comma-separated lists: one core (`list_items`) behind `delimited`, `delimited_after` and `separated`, so the
//! separator, trailing-comma and recovery rules exist once. Also `close_delimiter`, for single-item brackets.
//!
//! A list alternates between expecting an item and having just parsed one; see `list_items`. Separators are
//! required, a trailing `,` is allowed, a missing `,` between two items is reported and recovered as if present,
//! a bad item is reported by its own parser and skipped to the next `,` or the closer, and a missing closer is
//! reported where it belongs. What a list *means* (grouping vs tuple, empty payloads, `self` placement) is
//! validated by the caller afterwards, on the returned [`ParsedList`].

use crate::parser::ast::Delimited;
use crate::parser::error::{Expected, ParseError, ParseErrorKind};
use crate::parser::recovery::{RECOVERY_STOP, is_closer};
use crate::parser::{PResult, Parser};
use crate::scanner::Span;
use crate::scanner::TokenType as TokT;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Delim {
    Paren,
    Bracket,
    Brace,
    Angle,
    Pipe,
}

impl Delim {
    pub(super) fn open(self) -> TokT {
        match self {
            Delim::Paren => TokT::LeftParen,
            Delim::Bracket => TokT::LeftBracket,
            Delim::Brace => TokT::LeftBrace,
            Delim::Angle => TokT::Less,
            Delim::Pipe => TokT::Pipe,
        }
    }

    pub(super) fn close(self) -> TokT {
        match self {
            Delim::Paren => TokT::RightParen,
            Delim::Bracket => TokT::RightBracket,
            Delim::Brace => TokT::RightBrace,
            Delim::Angle => TokT::Greater,
            Delim::Pipe => TokT::Pipe,
        }
    }
}

/// A parsed list, with what the parser needs to validate its shape. `close` is always a real span: when the
/// closer is missing it's the point after the last token, and the error has already been reported.
#[derive(Debug, Clone)]
pub(super) struct ParsedList<T> {
    pub open: Span,
    pub items: Vec<T>,
    /// The last `,` when no item followed it. `(x)` vs `(x,)` differ only here.
    pub trailing: Option<Span>,
    pub close: Span,
    /// An item failed and was reported. `items` is then incomplete, so checks on the list's shape (empty
    /// payload, one-element tuple) would only repeat that error and are skipped.
    pub failed: bool,
}

impl<T> ParsedList<T> {
    pub(super) fn span(&self) -> Span {
        self.open.merge(self.close)
    }

    /// The AST form, once the shape has been validated.
    pub(super) fn into_ast(self) -> Delimited<T> {
        Delimited {
            open: self.open,
            items: self.items,
            close: self.close,
        }
    }
}

/// One kind of delimited list. Written at the call site, next to the item parser that `item_start` mirrors.
#[derive(Debug, Clone, Copy)]
pub(super) struct ListSpec {
    pub delim: Delim,
    /// Singular noun for one item ("argument"), used in messages.
    pub item: &'static str,
    /// FIRST set of an item. After an item, a token in this set means a forgotten `,`; any other token means
    /// the list is over.
    pub item_start: &'static [TokT],
}

impl ListSpec {
    const fn new(delim: Delim, item: &'static str, item_start: &'static [TokT]) -> Self {
        ListSpec {
            delim,
            item,
            item_start,
        }
    }

    pub(super) const fn paren(item: &'static str, item_start: &'static [TokT]) -> Self {
        Self::new(Delim::Paren, item, item_start)
    }

    pub(super) const fn bracket(item: &'static str, item_start: &'static [TokT]) -> Self {
        Self::new(Delim::Bracket, item, item_start)
    }

    pub(super) const fn brace(item: &'static str, item_start: &'static [TokT]) -> Self {
        Self::new(Delim::Brace, item, item_start)
    }

    pub(super) const fn angle(item: &'static str, item_start: &'static [TokT]) -> Self {
        Self::new(Delim::Angle, item, item_start)
    }

    pub(super) const fn pipe(item: &'static str, item_start: &'static [TokT]) -> Self {
        Self::new(Delim::Pipe, item, item_start)
    }
}

struct ListItems<T> {
    items: Vec<T>,
    trailing: Option<Span>,
    failed: bool,
    /// Recovery from a failed item stopped somewhere other than `,` or the closer: the closer error would only
    /// repeat the item error.
    broken: bool,
}

impl<'src> Parser<'src> {
    /// Parses `open (item (',' item)* ','?)? close`, starting at the opener. Never fails.
    pub(super) fn delimited<T>(
        &mut self,
        spec: ListSpec,
        item: impl FnMut(&mut Self) -> PResult<T>,
    ) -> ParsedList<T> {
        let open = self.advance_kind(spec.delim.open()).span;
        self.delimited_after(open, spec, item)
    }

    /// [`Self::delimited`] for a caller that had to consume the opener itself to decide what follows (`?(`,
    /// `[` before checking for `[:]`, `(` before telling a destructure from a tuple payload).
    pub(super) fn delimited_after<T>(
        &mut self,
        open: Span,
        spec: ListSpec,
        item: impl FnMut(&mut Self) -> PResult<T>,
    ) -> ParsedList<T> {
        let list = self.list_items(spec.item, spec.item_start, &[spec.delim.close()], item);
        let close = if list.broken {
            self.previous.span.point_after()
        } else {
            let after_item = !list.items.is_empty() && list.trailing.is_none();
            self.close_list(open, spec.delim, after_item)
        };
        ParsedList {
            open,
            items: list.items,
            trailing: list.trailing,
            close,
            failed: list.failed,
        }
    }

    /// An undelimited list, ending before any token in `until` (not consumed): `impl T: A, B {`. `noun` and
    /// `item_start` are as in [`ListSpec`].
    pub(super) fn separated<T>(
        &mut self,
        until: &[TokT],
        noun: &'static str,
        item_start: &'static [TokT],
        item: impl FnMut(&mut Self) -> PResult<T>,
    ) -> Vec<T> {
        self.list_items(noun, item_start, until, item).items
    }

    /// The list core, starting after the opener and stopping before the closer (`close`), which the caller
    /// handles. It alternates between two states:
    ///
    /// - Expecting an item (after the opener or a `,`): the closer ends the list (so `,)` ends it like `)`), a
    ///   boundary token (see [`Self::at_list_boundary`]) ends it with the closer missing, and anything else is
    ///   parsed as an item, so a bad item is reported by its own parser.
    /// - After an item: `,` expects the next item, a token in `item_start` is the next item with its `,`
    ///   forgotten (see [`Self::missing_separator`]), and anything else ends the list; if it isn't the closer,
    ///   the caller reports it missing.
    ///
    /// Every iteration consumes a token or ends the loop.
    fn list_items<T>(
        &mut self,
        noun: &'static str,
        item_start: &'static [TokT],
        close: &[TokT],
        mut item: impl FnMut(&mut Self) -> PResult<T>,
    ) -> ListItems<T> {
        // The opener's line, or for `separated` the line of the token before the first item.
        let start_line = self.previous.span.line;
        let mut list = ListItems {
            items: vec![],
            trailing: None,
            failed: false,
            broken: false,
        };
        loop {
            // Expecting an item.
            if self.check_any(close) || (!self.check_any(item_start) && self.at_list_boundary()) {
                break;
            }
            if self.check(TokT::Comma) {
                // `f(,)` / `f(a,,b)`
                self.errors
                    .push(self.err_unexpected(Expected::Item(noun), None));
                self.advance();
                continue;
            }
            list.trailing = None;
            match item(self) {
                Ok(t) => list.items.push(t),
                Err(e) => {
                    self.errors.push(e);
                    list.failed = true;
                    self.skip_to(&[&[TokT::Comma], close, RECOVERY_STOP]);
                    if !self.check(TokT::Comma) && !self.check_any(close) {
                        list.broken = true;
                        break;
                    }
                }
            }

            // After an item.
            if self.try_consume(TokT::Comma) {
                list.trailing = Some(self.previous.span);
                continue;
            }
            if self.check_any(item_start) && self.missing_separator(close, start_line) {
                let kind = ParseErrorKind::MissingSeparator {
                    item: noun,
                    found: self.found(),
                };
                self.report(kind, self.previous.span.point_after());
                continue;
            }
            break;
        }
        list
    }

    /// At an item position, a token that ends the list rather than starting a bad item: EOF, a statement
    /// boundary or a closer of an enclosing construct. Checked only for tokens outside `item_start`, since
    /// struct fields start with `public`.
    fn at_list_boundary(&self) -> bool {
        self.at_eof() || self.check_any(RECOVERY_STOP) || is_closer(self.current.kind)
    }

    /// After an item, whether the current token (one in `item_start`) is the next item with its `,` forgotten,
    /// rather than the code after a list whose closer is missing. While the list still fits on its first line,
    /// a token starting a new line is taken as the code after it (`f(a, b⏎g(c);`), unless it is directly
    /// followed by `,` or the closer (`f(a⏎b)`).
    fn missing_separator(&self, close: &[TokT], start_line: u32) -> bool {
        let at_line_break = self.at_line_start() && self.previous.span.line == start_line;
        !at_line_break || self.check_next(TokT::Comma) || close.contains(&self.next.kind)
    }

    /// Consumes the closer of `delim`. If it's missing, reports it with a label on `open`, skips (balanced, and
    /// not past a statement boundary) to the closer and consumes it if found. Never fails.
    ///
    /// There is no skip where it couldn't find the right closer: for `<>` and `|…|`, which aren't depth-tracked
    /// (`List<List<number> = []` would skip past the `=`), and when a list that fit on one line ended at a line
    /// break, where the next line is new code (`f(a, b⏎g(c);`).
    ///
    /// A closer glued to the next token is split off it (see [`glued_rest`]).
    pub(super) fn close_delimiter(&mut self, open: Span, delim: Delim) -> Span {
        self.close_list(open, delim, false)
    }

    /// [`Self::close_delimiter`] for a list. `after_item`: the list ended right after an item, so unless the
    /// current token ends the statement, a `,` is as likely a fix as the closer, and the error names both. Not for
    /// `<>` and `|…|`, whose next token always belongs to the code around them (`List<number> =`).
    fn close_list(&mut self, open: Span, delim: Delim, after_item: bool) -> Span {
        let close = delim.close();
        if self.check(close) {
            return self.advance().span;
        }
        if let Some(rest) = glued_rest(close, self.current.kind) {
            return self.split_current(close, rest).span;
        }
        let kind = ParseErrorKind::UnclosedDelimiter {
            open,
            close,
            found: self.found(),
            after_item: after_item
                && !matches!(delim, Delim::Angle | Delim::Pipe)
                && !self.at_line_start()
                && !self.at_list_boundary(),
        };
        self.errors
            .push(ParseError::new(kind, self.expected_span()));
        let at_line_break = self.at_line_start() && self.previous.span.line == open.line;
        if matches!(delim, Delim::Angle | Delim::Pipe) || at_line_break {
            return self.previous.span.point_after();
        }
        self.skip_to(&[&[close], RECOVERY_STOP]);
        if self.check(close) {
            self.advance().span
        } else {
            self.previous.span.point_after()
        }
    }
}

/// The rest of `found` when it is the closer `close` glued to the next token by the scanner's longest match:
/// `List<number>= []` lexes `>=`. The only closer that prefixes another token is `>`; `>>` would join here if
/// it ever becomes an operator.
fn glued_rest(close: TokT, found: TokT) -> Option<TokT> {
    match (close, found) {
        (TokT::Greater, TokT::GreaterEqual) => Some(TokT::Equal),
        _ => None,
    }
}
