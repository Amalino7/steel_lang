use crate::parser::ast::{Expr, Literal, Stmt, Type};
use crate::parser::error::ParserError;
use crate::parser::TokT;
use crate::parser::{check_token_type, match_token_type, Parser};

impl<'src> Parser<'src> {
    pub(super) fn declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::Let) {
            self.let_declaration()
        } else if match_token_type!(self, TokT::Func) {
            self.func_declaration()
        } else {
            self.statement()
        }
    }
    fn let_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if !match_token_type!(self, TokT::Identifier) {
            Err(self.error_current("Expected variable name."))
        } else {
            let name = self.previous_token.clone();
            let type_info = if match_token_type!(self, TokT::Colon) {
                self.consume(
                    TokT::Identifier,
                    "Expected the name of the variable type. Syntax: 'name: type'.",
                )?;
                if let Some(t) = Type::from_identifier(self.previous_token.clone()) {
                    t
                } else {
                    return Err(self.error_previous("Expected valid type identifier."));
                }
            } else {
                Type::Unknown // The type will be inferred by the compiler later on.
            };

            self.consume(TokT::Equal, "Expected '=' after variable name.")?;
            let expr = self.expression()?;
            self.consume(TokT::Semicolon, "Expected ';' after variable declaration.")?;
            Ok(Stmt::Let {
                identifier: name,
                value: expr,
                type_info,
                id: self.get_node_id(),
            })
        }
    }

    // TODO rethink how to refactor this function
    fn func_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if !match_token_type!(self, TokT::Identifier) {
            Err(self.error_current("Expected function name."))
        } else {
            let name = self.previous_token.clone();
            self.consume(TokT::LeftParen, "Expected '(' after function name.")?;
            let mut params = vec![];
            let mut param_types = vec![];

            if !check_token_type!(self, TokT::RightParen) {
                loop {
                    self.consume(
                        TokT::Identifier,
                        "Expected the name of the function parameter.",
                    )?;
                    params.push(self.previous_token.clone());
                    self.consume(
                        TokT::Colon,
                        "Expected ':' after parameter name. Syntax: 'name: type",
                    )?;
                    self.consume(
                        TokT::Identifier,
                        "Expected the type after the function parameter name. Syntax: 'name: type'.",
                    )?;

                    if let Some(t) = Type::from_identifier(self.previous_token.clone()) {
                        param_types.push(t);
                    } else {
                        return Err(self.error_previous(
                            "Expected valid type identifier. Example: 'number, string, void'.",
                        ));
                    }

                    if !match_token_type!(self, TokT::Comma) {
                        break;
                    }
                }
            }

            self.consume(TokT::RightParen, "Expected ')' after parameters.")?;

            let return_type = if match_token_type!(self, TokT::Colon) {
                self.consume(
                    TokT::Identifier,
                    "Expected the name of the return type of the function.",
                )?;
                if let Some(t) = Type::from_identifier(self.previous_token.clone()) {
                    t
                } else {
                    return Err(self.error_previous("Expected valid type identifier"));
                }
            } else {
                Type::Void
            };

            self.consume(TokT::LeftBrace, "Expected '{' before function body.")?;
            let mut body = vec![];

            while !match_token_type!(self, TokT::RightBrace) {
                body.push(self.declaration()?);
            }

            Ok(Stmt::Function {
                type_: Type::Function {
                    param_types,
                    return_type: Box::new(return_type),
                },
                name,
                params,
                body,
                id: self.get_node_id(),
            })
        }
    }

    // fn type_block(&mut self) -> Result<Type, ParserError<'src>> {
    //     match self.current_token.token_type {
    //         TokenType::Func => {}
    //         TokenType::Identifier => {
    //             self.consume(
    //                 TokT::Identifier,
    //                 "Expected the name of the return type of the function.",
    //             )?;
    //
    //             if let Some(t) = Type::from_identifier(self.previous_token.clone()) {
    //                 Ok(t)
    //             } else {
    //                 return Err(self.error_previous("Expected valid type identifier"));
    //             }
    //         }
    //         _ => Err(self.error_current("Expected the name of the return type of the function.")),
    //     }
    // }

    fn statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if check_token_type!(self, TokT::LeftBrace) {
            self.block()
        } else if match_token_type!(self, TokT::If) {
            self.if_statement()
        } else if match_token_type!(self, TokT::While) {
            self.while_statement()
        } else if match_token_type!(self, TokT::Return) {
            let val = if !match_token_type!(self, TokT::Semicolon) {
                let expr = self.expression()?;
                self.consume(TokT::Semicolon, "Expected ';' after return value.")?;
                expr
            } else {
                Expr::Literal {
                    literal: Literal::Void,
                    line: self.previous_token.line,
                }
            };
            Ok(Stmt::Return(val))
        } else {
            let expr = self.expression()?;
            self.consume(TokT::Semicolon, "Expected ';' after expression.")?;
            Ok(Stmt::Expression(expr))
        }
    }
    fn block(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::LeftBrace, "Expected '{' before block.")?;
        let mut statements = vec![];
        while !check_token_type!(self, TokT::RightBrace) {
            statements.push(self.declaration()?);
        }

        self.consume(TokT::RightBrace, "Expected '}' after block.")?;
        Ok(Stmt::Block(statements))
    }
    fn if_statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let condition = self.expression()?;
        let then_branch = self.block()?;
        let mut else_branch = None;
        if match_token_type!(self, TokT::Else) {
            if match_token_type!(self, TokT::If) {
                else_branch = Some(self.if_statement()?);
            } else {
                else_branch = Some(self.block()?);
            }
        }
        Ok(Stmt::If {
            condition,
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(|e| Box::new(e)),
        })
    }
    fn while_statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let condition = self.expression()?;
        let body = self.block()?;
        Ok(Stmt::While {
            condition,
            body: Box::new(body),
        })
    }
}
