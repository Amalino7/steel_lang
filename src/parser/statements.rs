//! Statements: declaration/statement dispatch, the import header, `while`, and the one block parser shared by
//! function bodies, `while`, `if`/`else`, match arms and block expressions.

use crate::parser::ast::{Expr, ImportSegment, ImportStmt, ImportType, Stmt};
use crate::parser::declarations::Visibility;
use crate::parser::error::ParserError;
use crate::parser::lists::{Delim, ListSpec};
use crate::parser::{PResult, Parser, TokT};
use crate::scanner::{Span, Token};

/// A `{ … }` block. Becomes a `Stmt::Block` or an `Expr::Block` depending on where it appears.
pub(super) struct Block<'src> {
    pub open: Token<'src>,
    pub stmts: Vec<Stmt<'src>>,
    pub tail: Option<Box<Expr<'src>>>,
    pub close: Span,
}

impl<'src> Block<'src> {
    /// For statement blocks (`allow_tail: false`), which never have a tail.
    pub fn into_stmt(self) -> Stmt<'src> {
        debug_assert!(self.tail.is_none(), "statement block with a tail");
        Stmt::Block {
            open_brace: self.open,
            body: self.stmts,
            close_brace: Token::new(TokT::RightBrace, self.close, "}"),
        }
    }

    pub fn into_expr(self) -> Expr<'src> {
        Expr::Block {
            brace_token: self.open,
            body: self.stmts,
            tail: self.tail,
        }
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
            BlockItem::Tail(expr) => Stmt::Expression(expr),
        }
    }
}

impl<'src> Parser<'src> {
    /// One top-level item: a declaration or a statement.
    pub(super) fn top_level_item(&mut self) -> PResult<'src, Stmt<'src>> {
        self.declaration_or_statement(true, false)
            .map(BlockItem::into_stmt)
    }

    /// `public` is parsed here and passed to the declaration it prefixes; constructs that don't allow it report
    /// it. Anything that isn't a declaration goes to [`Self::statement`] with `allow_tail`.
    fn declaration_or_statement(
        &mut self,
        top_level: bool,
        allow_tail: bool,
    ) -> PResult<'src, BlockItem<'src>> {
        let vis = self.visibility();
        // A nested `public` is reported once, here, and not again by the construct it prefixes.
        let unreported = if top_level {
            vis
        } else {
            self.forbid_visibility(vis, "`public` is only allowed on top-level declarations.");
            Visibility::Private
        };

        let declaration = match self.current.kind {
            TokT::Let => self.let_declaration(vis),
            TokT::Func => self.func_declaration(vis, false),
            TokT::Enum => self.enum_declaration(vis),
            TokT::Struct => self.struct_declaration(vis),
            TokT::Interface => self.interface_declaration(vis),
            TokT::Extern => self.extern_func_declaration(vis, false),
            TokT::Impl => {
                self.forbid_visibility(
                    unreported,
                    "`impl` blocks are always visible; mark individual methods `public`.",
                );
                self.impl_block()
            }
            TokT::Import => {
                let import = self.import_statement()?;
                Err(ParserError::ParseError {
                    token: import.keyword,
                    message: "imports must come before all other declarations".to_string(),
                })
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
    fn statement(&mut self, allow_tail: bool) -> PResult<'src, BlockItem<'src>> {
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
                return Ok(BlockItem::Stmt(Stmt::Expression(expr)));
            }
        };
        if allow_tail && self.check(TokT::RightBrace) {
            return Ok(BlockItem::Tail(expr));
        }
        self.try_consume(TokT::Semicolon);
        Ok(BlockItem::Stmt(Stmt::Expression(expr)))
    }

    /// `{ stmt* tail? }`. Fails only if the current token isn't `{`. E
    /// Errors inside are recovered per statement.
    /// Function and `while` bodies pass `allow_tail: false`, since there a final expression without `;` is an error.
    pub(super) fn block(
        &mut self,
        allow_tail: bool,
        after: &'static str,
    ) -> PResult<'src, Block<'src>> {
        let open = self.expect(TokT::LeftBrace, after)?;
        let mut items = self.sequence(&[TokT::RightBrace], Self::recover_stmt, |p| {
            p.declaration_or_statement(false, allow_tail)
        });
        let close = self.close_delimiter(open.span, Delim::Brace);
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
            open,
            stmts,
            tail,
            close,
        })
    }

    pub(super) fn block_expr(&mut self) -> PResult<'src, Expr<'src>> {
        self.block(true, "block").map(Block::into_expr)
    }

    fn while_statement(&mut self) -> PResult<'src, Stmt<'src>> {
        self.advance_kind(TokT::While);
        let condition = self.expression()?;
        let body = self.block(false, "while condition")?;
        Ok(Stmt::While {
            condition,
            body: Box::new(body.into_stmt()),
        })
    }

    /// Parses the import header: every leading `import`. A malformed import is reported and skipped, so the
    /// well-formed ones around it are still returned.
    pub fn parse_imports(&mut self) -> Vec<ImportStmt<'src>> {
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

    fn header_import(&mut self) -> PResult<'src, ImportStmt<'src>> {
        let vis = self.visibility();
        self.forbid_visibility(vis, "re-exports are not supported.");
        self.import_statement()
    }

    fn import_statement(&mut self) -> PResult<'src, ImportStmt<'src>> {
        let keyword = self.advance_kind(TokT::Import);
        let segment = self.import_segment(true)?;
        self.expect(TokT::Semicolon, "import")?;
        Ok(ImportStmt {
            keyword,
            segment,
            semicolon: self.previous,
        })
    }

    /// `top_level` segments (the ones directly after `import`) must name an item: `module/item`.
    /// Segments inside a `{ }` group may be a bare item.
    fn import_segment(&mut self, top_level: bool) -> PResult<'src, ImportSegment<'src>> {
        let mut path = vec![self.expect_ident("import")?];

        let mut after_slash = false;
        while self.try_consume(TokT::Slash) {
            after_slash = true;
            if self.check(TokT::LeftBrace) {
                break;
            }
            path.push(self.expect_ident("'/'")?);
            after_slash = false;
        }

        let import_type = if self.check(TokT::LeftBrace) {
            if !after_slash {
                return Err(self.err_expected(TokT::Slash, "module path"));
            }
            let open = self.advance().span;
            if self.try_consume(TokT::Star) {
                let star = self.previous;
                self.close_delimiter(open, Delim::Brace);
                ImportType::Wildcard(star)
            } else {
                let group = self.delimited_after(
                    open,
                    ListSpec::brace("import", &[TokT::Identifier]),
                    |p| p.import_segment(false),
                );
                if group.items.is_empty() && !group.failed {
                    self.errors.push(ParserError::Invalid {
                        span: group.open.to(group.close),
                        message: "Import group must not be empty.".to_string(),
                    });
                }
                ImportType::Group {
                    options: group.items,
                }
            }
        } else {
            if top_level && path.len() < 2 {
                return Err(self.err_expected(TokT::Slash, "module name ('module/item')"));
            }
            let terminator = path.pop().expect("path starts with an identifier");
            if self.try_consume(TokT::As) {
                let alias = self.expect_ident("'as'")?;
                ImportType::Alias { terminator, alias }
            } else {
                ImportType::Simple { terminator }
            }
        };

        for element in path.iter() {
            if !element.lexeme.is_ascii() {
                self.errors.push(ParserError::ParseError {
                    token: *element,
                    message: "Expected ASCII identifier in import path.".to_string(),
                })
            }
        }

        Ok(ImportSegment { path, import_type })
    }
}
