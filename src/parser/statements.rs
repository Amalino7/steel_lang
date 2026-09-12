use crate::parser::ast::{ImportSegment, ImportStmt, ImportType, Stmt};
use crate::parser::error::ParserError;
use crate::parser::{Parser, TokT, check_token_type, match_token_type};

impl<'src> Parser<'src> {
    pub(super) fn declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::Let) {
            self.let_declaration()
        } else if match_token_type!(self, TokT::Enum) {
            self.enum_declaration()
        } else if match_token_type!(self, TokT::Func) {
            self.func_declaration(false)
        } else if match_token_type!(self, TokT::Import) {
            self.import_statement()
        } else if match_token_type!(self, TokT::Struct) {
            self.struct_declaration()
        } else if match_token_type!(self, TokT::Impl) {
            self.impl_block()
        } else if match_token_type!(self, TokT::Interface) {
            self.interface_declaration()
        } else if match_token_type!(self, TokT::Extern) {
            self.consume(TokT::Func, "Expected 'func' after 'extern'.")?;
            self.extern_func_declaration(false)
        } else {
            self.statement()
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
    fn import_statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let import = self.previous_token.clone();
        let segment = self.import_segment()?;
        self.consume(TokT::Semicolon, "Expected ';' after import statement")?;
        Ok(Stmt::Import(ImportStmt {
            keyword: import,
            segment,
        }))
    }

    fn import_segment(&mut self) -> Result<ImportSegment<'src>, ParserError<'src>> {
        self.consume(TokT::Identifier, "Expected import name")?;
        let mut path = vec![];
        path.push(self.previous_token.clone());

        while match_token_type!(self, TokT::Slash) && match_token_type!(self, TokT::Identifier) {
            path.push(self.previous_token.clone());
        }

        let import_type = if match_token_type!(self, TokT::LeftBrace) {
            if match_token_type!(self, TokT::Star) {
                self.consume(TokT::RightBrace, "Expected '}' after wildcard import.")?;
                ImportType::Wildcard
            } else {
                let mut options = vec![];
                while !check_token_type!(self, TokT::RightBrace) {
                    options.push(self.import_segment()?);
                    match_token_type!(self, TokT::Comma);
                }
                self.consume(TokT::RightBrace, "Expected '}' after group import.")?;
                ImportType::Group { options }
            }
        } else if match_token_type!(self, TokT::As) {
            self.consume(TokT::Identifier, "Expected alias after 'as'.")?;
            let term = path.pop().unwrap();
            ImportType::Alias {
                terminator: term,
                alias: self.previous_token.clone(),
            }
        } else {
            let term = path.pop().unwrap();
            ImportType::Simple { terminator: term }
        };

        Ok(ImportSegment { path, import_type })
    }

    pub(super) fn block(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::LeftBrace, "Expected '{' before block.")?;
        let mut statements = vec![];
        while !check_token_type!(self, TokT::RightBrace) {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.synchronize();
                    self.errors.push(e);
                    if check_token_type!(self, TokT::EOF) {
                        break;
                    }
                }
            }
        }

        self.consume(TokT::RightBrace, "Expected '}' after block.")?;
        let brace_token = self.previous_token.clone();
        Ok(Stmt::Block {
            body: statements,
            brace_token,
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
        )
    }
}
