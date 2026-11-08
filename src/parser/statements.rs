//! Statements: declaration/statement dispatch, the import header, `while`, and the one block parser shared by
//! function bodies, `while`, `if`/`else`, match arms and block expressions.

use crate::parser::ast::{self, Expr, ExprKind, Import, ImportKind, ImportTree, Stmt, Visibility};
use crate::parser::declarations::FnContext;
use crate::parser::error::{ParseError, ParseErrorKind};
use crate::parser::lists::{Delim, ListSpec};
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

impl<'src> Parser<'src> {
    /// One top-level item: a declaration or a statement.
    pub(super) fn top_level_item(&mut self) -> PResult<Stmt<'src>> {
        self.declaration_or_statement(true, false)
            .map(BlockItem::into_stmt)
    }

    /// `public` is parsed here and passed to the declaration it prefixes; constructs that don't allow it report
    /// it. Anything that isn't a declaration goes to [`Self::statement`] with `allow_tail`.
    fn declaration_or_statement(
        &mut self,
        top_level: bool,
        allow_tail: bool,
    ) -> PResult<BlockItem<'src>> {
        let start = self.start_span();
        let vis = self.visibility();
        // A nested `public` is reported once, here, and not again by the construct it prefixes.
        let unreported = if top_level {
            vis
        } else {
            self.forbid_visibility(vis, "only top-level declarations can be public.");
            Visibility::Private
        };

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
                    unreported,
                    "`impl` blocks are always visible; mark individual methods `public`.",
                );
                self.impl_block(start).map(|d| Stmt::Impl(Box::new(d)))
            }
            TokT::Import => {
                let keyword = self.current.span;
                self.import_statement()?;
                Err(ParseError::new(ParseErrorKind::ImportNotAtTop, keyword))
            }
            _ => {
                self.forbid_visibility(unreported, "expected a declaration after `public`.");
                return self.statement(allow_tail);
            }
        };
        declaration.map(BlockItem::Stmt)
    }

    /// A non-declaration statement.
    /// A statement that *starts* with `{`, `if` or `match` is exactly that
    /// construct, with no binary or postfix continuation, and needs no `;`. Everything else is `expression ;`.
    /// With `allow_tail`, an expression directly before `}` is the block's value instead.
    fn statement(&mut self, allow_tail: bool) -> PResult<BlockItem<'src>> {
        let expr = match self.current.kind {
            TokT::While => return self.while_statement().map(BlockItem::Stmt),
            TokT::LeftBrace => self.block_expr()?,
            TokT::If => self.if_expr()?,
            TokT::Match => self.match_expr()?,
            _ => {
                let expr = self.expression()?;
                if allow_tail && self.check(TokT::RightBrace) {
                    return Ok(BlockItem::Tail(expr));
                }
                self.expect_semi("expression")?;
                return Ok(BlockItem::Stmt(Stmt::Expr(expr)));
            }
        };
        if allow_tail && self.check(TokT::RightBrace) {
            return Ok(BlockItem::Tail(expr));
        }
        self.try_consume(TokT::Semicolon);
        Ok(BlockItem::Stmt(Stmt::Expr(expr)))
    }

    /// `{ stmt* tail? }`. Fails only if the current token isn't `{`. Errors inside are recovered per statement.
    /// Function and `while` bodies pass `allow_tail: false`, since there a final expression without `;` is an
    /// error.
    pub(super) fn block(&mut self, allow_tail: bool, after: &'static str) -> PResult<Block<'src>> {
        let open = self.expect(TokT::LeftBrace, after)?.span;
        let mut items = self.sequence(&[TokT::RightBrace], Self::recover_stmt, |p| {
            p.declaration_or_statement(false, allow_tail)
        });
        let close = self.close_delimiter(open, Delim::Brace);
        let tail = match items.pop() {
            Some(BlockItem::Tail(expr)) => Some(Box::new(expr)),
            Some(item) => {
                items.push(item);
                None
            }
            None => None,
        };
        let stmts = items.into_iter().map(BlockItem::into_stmt).collect();
        Ok(Block {
            stmts,
            tail,
            span: open.merge(close),
        })
    }

    pub(super) fn block_expr(&mut self) -> PResult<Expr<'src>> {
        self.block(true, "block").map(Block::into_expr)
    }

    /// Parses the import header: every leading `import`. A malformed import is reported and skipped, so the
    /// well-formed ones around it are still returned.
    pub fn parse_imports(&mut self) -> Vec<Import<'src>> {
        let mut imports = vec![];
        while self.at_import() {
            match self.header_import() {
                Ok(import) => imports.push(import),
                Err(e) => {
                    self.errors.push(e);
                    self.recover_top_level();
                }
            }
        }
        imports
    }

    fn at_import(&self) -> bool {
        self.check(TokT::Import) || (self.check(TokT::Public) && self.check_next(TokT::Import))
    }

    fn header_import(&mut self) -> PResult<Import<'src>> {
        let vis = self.visibility();
        self.forbid_visibility(vis, "re-exports are not supported.");
        self.import_statement()
    }

    fn import_statement(&mut self) -> PResult<Import<'src>> {
        let start = self.advance_kind(TokT::Import).span;
        let tree = self.import_tree(true)?;
        self.expect(TokT::Semicolon, "import")?;
        Ok(Import {
            tree,
            span: self.span_from(start),
        })
    }

    /// `top_level` trees (the ones directly after `import`) must name an item: `module/item`.
    /// Trees inside a `{ }` group may be a bare item.
    fn import_tree(&mut self, top_level: bool) -> PResult<ImportTree<'src>> {
        let start = self.start_span();
        let mut path = vec![self.expect_ident("import")?];

        let mut after_slash = false;
        while self.try_consume(TokT::Slash) {
            after_slash = true;
            if self.check(TokT::LeftBrace) {
                break;
            }
            path.push(self.expect_ident("`/`")?);
            after_slash = false;
        }

        let kind = if self.check(TokT::LeftBrace) {
            if !after_slash {
                return Err(self.err_expected(TokT::Slash, "module path"));
            }
            let open = self.advance().span;
            if self.try_consume(TokT::Star) {
                let star = self.previous.span;
                self.close_delimiter(open, Delim::Brace);
                ImportKind::Wildcard(star)
            } else {
                let group = self.delimited_after(
                    open,
                    ListSpec::brace("import", &[TokT::Identifier]),
                    |p| p.import_tree(false),
                );
                if group.items.is_empty() && !group.failed {
                    self.errors.push(ParseError::new(
                        ParseErrorKind::EmptyImportGroup,
                        group.span(),
                    ));
                }
                ImportKind::Group(group.into_ast())
            }
        } else {
            if top_level && path.len() < 2 {
                return Err(self.err_expected(TokT::Slash, "module name (`module/item`)"));
            }
            let name = path.pop().expect("path starts with an identifier");
            if self.try_consume(TokT::As) {
                let alias = self.expect_ident("`as`")?;
                ImportKind::Alias { name, alias }
            } else {
                ImportKind::Item { name }
            }
        };

        for segment in &path {
            if !segment.name.is_ascii() {
                self.report(ParseErrorKind::NonAsciiImportPath, segment.span)
            }
        }

        Ok(ImportTree {
            path,
            kind,
            span: self.span_from(start),
        })
    }
}
