//! Statements: declaration/statement dispatch, the import header, and the one block parser shared by
//! function bodies, `while`, `if`/`else`, match arms and block expressions.

use crate::parser::ast::{self, Expr, ExprKind, Stmt, Visibility};
use crate::parser::declarations::FnContext;
use crate::parser::error::ParseErrorKind;
use crate::parser::lists::Delim;
use crate::parser::{PResult, Parser, TokT};
use crate::scanner::Span;

/// A parsed `{ … }`. Becomes an [`ast::Block`] (function and `while` bodies) or a block expression.
pub(super) struct Block<'src> {
    stmts: Vec<Stmt<'src>>,
    tail: Option<Box<Expr<'src>>>,
    span: Span,
}

impl<'src> Block<'src> {
    /// For statement blocks (`allow_tail: false`), which never have a tail.
    pub fn into_body(self) -> ast::Block<'src> {
        debug_assert!(self.tail.is_none(), "statement block with a tail");
        ast::Block {
            stmts: self.stmts,
            span: self.span,
        }
    }

    pub fn into_expr(self) -> Expr<'src> {
        Expr::new(
            ExprKind::Block {
                stmts: self.stmts,
                tail: self.tail,
            },
            self.span,
        )
    }
}

/// One item of a block: a statement, or the final expression without `;` that is the block's value.
enum BlockItem<'src> {
    Stmt(Stmt<'src>),
    Tail(Expr<'src>),
}

impl<'src> BlockItem<'src> {
    fn into_stmt(self) -> Stmt<'src> {
        match self {
            BlockItem::Stmt(stmt) => stmt,
            BlockItem::Tail(expr) => Stmt::Expr(expr),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum StmtCtx {
    TopLevel,
    Body,
    Value,
}

impl<'src> Parser<'src> {
    /// One top-level item: a declaration or a statement.
    pub(super) fn top_level_item(&mut self) -> PResult<Stmt<'src>> {
        self.statement(StmtCtx::TopLevel).map(BlockItem::into_stmt)
    }

    /// `public` is parsed here and passed to the declaration it prefixes; constructs that don't allow it report
    /// it. Anything that isn't a declaration goes to [`Self::statement`] with `allow_tail`.
    fn statement(&mut self, ctx: StmtCtx) -> PResult<BlockItem<'src>> {
        let start = self.start_span();
        let mut vis = self.visibility();
        // A nested `public` is reported once, here, and not again by the construct it prefixes.
        if !ctx.allows_public() {
            self.forbid_visibility(vis, "only top-level declarations can be public.");
            vis = Visibility::Private;
        }

        let declaration = match self.current.kind {
            TokT::Let => self
                .let_declaration(vis, start)
                .map(|d| Stmt::Let(Box::new(d))),
            TokT::Func => self
                .func_declaration(vis, start, FnContext::Free)
                .map(|d| Stmt::Fn(Box::new(d))),
            TokT::Extern => self
                .extern_func_declaration(vis, start, FnContext::Free)
                .map(|d| Stmt::Fn(Box::new(d))),
            TokT::Enum => self
                .enum_declaration(vis, start)
                .map(|d| Stmt::Enum(Box::new(d))),
            TokT::Struct => self
                .struct_declaration(vis, start)
                .map(|d| Stmt::Struct(Box::new(d))),
            TokT::Interface => self
                .interface_declaration(vis, start)
                .map(|d| Stmt::Interface(Box::new(d))),
            TokT::Impl => {
                self.forbid_visibility(
                    vis,
                    "`impl` blocks are always visible; mark individual methods `public`.",
                );
                self.impl_block(start).map(|d| Stmt::Impl(Box::new(d)))
            }
            TokT::Import => {
                let keyword = self.current.span;
                let import = self.import_statement()?;
                self.report(ParseErrorKind::ImportNotAtTop, keyword);
                Ok(Stmt::Expr(Expr::new(ExprKind::Error, import.span)))
            }
            TokT::While => {
                self.forbid_visibility(vis, "expected a declaration after `public`.");
                self.while_statement()
            }
            _ => {
                self.forbid_visibility(vis, "expected a declaration after `public`.");
                return self.expr_statement(ctx.allows_tail());
            }
        };
        declaration.map(BlockItem::Stmt)
    }

    /// An expression statement.
    /// Constructs that *end* with `}` are considered one item and don't need `;`.
    /// Everything else is `expression ;`.
    /// With `allow_tail`, an expression directly before `}` is the block's value instead.
    fn expr_statement(&mut self, allow_tail: bool) -> PResult<BlockItem<'src>> {
        let needs_semi = !matches!(self.current.kind, TokT::LeftBrace | TokT::If | TokT::Match);
        let expr = match self.current.kind {
            TokT::LeftBrace => self.block_expr()?,
            TokT::If => self.if_expr()?,
            TokT::Match => self.match_expr()?,
            _ => self.expression()?,
        };

        if allow_tail && self.check(TokT::RightBrace) {
            return Ok(BlockItem::Tail(expr));
        }

        if needs_semi {
            self.expect_semi("expression")?;
        } else {
            self.try_consume(TokT::Semicolon);
        }
        Ok(BlockItem::Stmt(Stmt::Expr(expr)))
    }

    /// `{ stmt* tail? }`. Fails only if the current token isn't `{`. Errors inside are recovered per statement.
    /// Function and `while` bodies pass `allow_tail: false`, since there a final expression without `;` is an
    /// error.
    pub(super) fn block(&mut self, stmt_ctx: StmtCtx, after: &'static str) -> PResult<Block<'src>> {
        let open = self.expect(TokT::LeftBrace, after)?.span;
        let mut items = self.sequence(&[TokT::RightBrace], Self::recover_stmt, |p| {
            p.statement(stmt_ctx)
        });
        let close = self.close_delimiter(open, Delim::Brace);
        let tail = match items.pop() {
            Some(BlockItem::Tail(expr)) => Some(Box::new(expr)),
            other => {
                items.extend(other);
                None
            }
        };
        let stmts = items.into_iter().map(BlockItem::into_stmt).collect();
        Ok(Block {
            stmts,
            tail,
            span: open.merge(close),
        })
    }

    pub(super) fn block_expr(&mut self) -> PResult<Expr<'src>> {
        self.block(StmtCtx::Value, "block").map(Block::into_expr)
    }
}

impl StmtCtx {
    fn allows_public(self) -> bool {
        self == Self::TopLevel
    }
    fn allows_tail(self) -> bool {
        self == Self::Value
    }
}
