use crate::parser::ast::{ImportSegment, ImportStmt, ImportType, Stmt};
use crate::parser::error::ParserError;
use crate::parser::{Parser, TokT, check_next_token_type, check_token_type, match_token_type};
use crate::scanner::Token;

impl<'src> Parser<'src> {
    pub(super) fn declaration(&mut self, top_level: bool) -> Result<Stmt<'src>, ParserError<'src>> {
        let is_public = match_token_type!(self, TokT::Public);
        let prev_token = self.previous_token.clone();
        if is_public && !top_level {
            self.errors.push(ParserError::ParseError {
                token: prev_token.clone(),
                message: "`public` is only allowed on top-level declarations.".to_string(),
            });
        }
        // Already reported above when nested; don't report a second error for the same token.
        let report = is_public && top_level;

        if match_token_type!(self, TokT::Let) {
            self.let_declaration(is_public)
        } else if match_token_type!(self, TokT::Enum) {
            self.enum_declaration(is_public)
        } else if match_token_type!(self, TokT::Func) {
            self.func_declaration(is_public, false)
        } else if match_token_type!(self, TokT::Import) {
            // Imports are parsed only initially.
            let keyword = self.previous_token.clone();
            self.import_statement()?;
            Err(ParserError::ParseError {
                token: keyword,
                message: "imports must come before all other declarations".to_string(),
            })
        } else if match_token_type!(self, TokT::Struct) {
            self.struct_declaration(is_public)
        } else if match_token_type!(self, TokT::Impl) {
            self.public_err(
                report,
                prev_token,
                "`impl` blocks are always visible; mark individual methods `public`.",
            );
            self.impl_block()
        } else if match_token_type!(self, TokT::Interface) {
            self.interface_declaration(is_public)
        } else if match_token_type!(self, TokT::Extern) {
            self.consume(TokT::Func, "Expected 'func' after 'extern'.")?;
            self.extern_func_declaration(is_public, false)
        } else {
            self.public_err(report, prev_token, "expected a declaration after `public`.");
            self.statement()
        }
    }

    fn public_err(&mut self, report: bool, token: Token<'src>, message: &str) {
        if report {
            self.errors.push(ParserError::ParseError {
                token,
                message: format!("`public` is not allowed here: {message}"),
            })
        }
    }

    pub(super) fn statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::LeftBrace) {
            Ok(Stmt::Expression(self.parse_block_expr()?))
        } else if match_token_type!(self, TokT::If) {
            Ok(Stmt::Expression(self.parse_if_expr()?))
        } else if match_token_type!(self, TokT::While) {
            self.while_statement()
        } else if match_token_type!(self, TokT::Match) {
            self.parse_match_expr().map(Stmt::Expression)
        } else {
            let expr = self.expression()?;
            self.consume(TokT::Semicolon, "Expected ';' after expression.")?;
            Ok(Stmt::Expression(expr))
        }
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
                    self.synchronize_top_level();
                }
            }
        }
        imports
    }

    fn at_import(&self) -> bool {
        check_token_type!(self, TokT::Import)
            || (check_token_type!(self, TokT::Public) && check_next_token_type!(self, TokT::Import))
    }

    fn header_import(&mut self) -> Result<ImportStmt<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::Public) {
            self.errors.push(ParserError::ParseError {
                token: self.previous_token.clone(),
                message: "`public` is not allowed here: re-exports are not supported.".to_string(),
            });
        }
        self.consume(TokT::Import, "Expected 'import'.")?;
        self.import_statement()
    }

    fn import_statement(&mut self) -> Result<ImportStmt<'src>, ParserError<'src>> {
        let keyword = self.previous_token.clone();
        let segment = self.import_segment(true)?;
        self.consume(TokT::Semicolon, "Expected ';' after import statement")?;
        Ok(ImportStmt {
            keyword,
            segment,
            semicolon: self.previous_token.clone(),
        })
    }

    /// `top_level` segments (the ones directly after `import`) must name an item: `module/item`.
    /// Segments inside a `{ }` group may be a bare item.
    fn import_segment(
        &mut self,
        top_level: bool,
    ) -> Result<ImportSegment<'src>, ParserError<'src>> {
        self.consume(TokT::Identifier, "Expected import name")?;
        let mut path = vec![self.previous_token.clone()];

        while match_token_type!(self, TokT::Slash) {
            if match_token_type!(self, TokT::Identifier) {
                path.push(self.previous_token.clone());
            } else if check_token_type!(self, TokT::LeftBrace) {
                break;
            } else {
                return Err(self.error_current("Expected an identifier or '{' after '/'."));
            }
        }
        let after_slash = self.previous_token.token_type == TokT::Slash;

        let import_type = if check_token_type!(self, TokT::LeftBrace) {
            if !after_slash {
                return Err(self.error_current("Expected '/' before '{' in import."));
            }
            self.advance()?;
            if match_token_type!(self, TokT::Star) {
                let star = self.previous_token.clone();
                self.consume(TokT::RightBrace, "Expected '}' after wildcard import.")?;
                ImportType::Wildcard(star)
            } else {
                let mut options = vec![];
                while !check_token_type!(self, TokT::RightBrace) {
                    options.push(self.import_segment(false)?);
                    if !match_token_type!(self, TokT::Comma) {
                        break;
                    }
                }
                if options.is_empty() {
                    // Consume the brace so recovery doesn't stop on it.
                    let err = self.error_current("Import group must not be empty.");
                    self.advance()?;
                    return Err(err);
                }
                self.consume(TokT::RightBrace, "Expected ',' or '}' in import group.")?;
                ImportType::Group { options }
            }
        } else {
            if top_level && path.len() < 2 {
                return Err(self.error_previous("Expected 'module/item' in import."));
            }
            let terminator = path.pop().expect("path starts with an identifier");
            if match_token_type!(self, TokT::As) {
                self.consume(TokT::Identifier, "Expected alias after 'as'.")?;
                ImportType::Alias {
                    terminator,
                    alias: self.previous_token.clone(),
                }
            } else {
                ImportType::Simple { terminator }
            }
        };

        for element in path.iter() {
            if !element.lexeme.is_ascii() {
                self.errors.push(ParserError::ParseError {
                    token: element.clone(),
                    message: "Expected ASCII identifier in import path.".to_string(),
                })
            }
        }

        Ok(ImportSegment { path, import_type })
    }

    pub(super) fn block(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::LeftBrace, "Expected '{' before block.")?;
        let open_brace = self.previous_token.clone();
        let mut statements = vec![];
        while !check_token_type!(self, TokT::RightBrace) {
            match self.declaration(false) {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.synchronize();
                    self.errors.push(e);
                    if self.at_eof() {
                        break;
                    }
                }
            }
        }

        self.consume(TokT::RightBrace, "Expected '}' after block.")?;
        Ok(Stmt::Block {
            open_brace,
            body: statements,
            close_brace: self.previous_token.clone(),
        })
    }

    fn while_statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let condition = self.expression();
        let condition = condition?;

        let body = self.block()?;
        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }

    pub(crate) fn is_stmt_start(&self) -> bool {
        let current_token = self.current_token.clone();
        matches!(
            current_token.token_type,
            TokT::Let
                | TokT::Func
                | TokT::Struct
                | TokT::Impl
                | TokT::Interface
                | TokT::Extern
                | TokT::Enum
                | TokT::While
                | TokT::Public
        )
    }
}
