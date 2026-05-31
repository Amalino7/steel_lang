use crate::parser::ast::Stmt;
use crate::parser::error::ParserError;
use crate::parser::{check_token_type, match_token_type, Parser, TokT};

impl<'src> Parser<'src> {
    pub(super) fn declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::Let) {
            self.let_declaration()
        } else if match_token_type!(self, TokT::Enum) {
            self.enum_declaration()
        } else if match_token_type!(self, TokT::Func) {
            self.func_declaration(false)
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
