use crate::parser::TokT;
use crate::parser::ast::{Import, ImportKind, ImportTree};
use crate::parser::error::ParseErrorKind;
use crate::parser::lists::{Delim, ListSpec};
use crate::parser::{PResult, Parser};

impl<'src> Parser<'src> {
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

    pub(super) fn import_statement(&mut self) -> PResult<Import<'src>> {
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
                    self.report(ParseErrorKind::EmptyImportGroup, group.span());
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
