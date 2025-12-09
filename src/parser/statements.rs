use crate::parser::ast::{Expr, Literal, Stmt, TypeAst};
use crate::parser::error::ParserError;
use crate::parser::TokT;
use crate::parser::{check_token_type, match_token_type, Parser};
use crate::token::{Token, TokenType};

#[derive(PartialEq)]
enum FuncContext<'src> {
    Func,
    Method(Token<'src>),
}

impl<'src> Parser<'src> {
    pub(super) fn declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::Let) {
            self.let_declaration()
        } else if match_token_type!(self, TokT::Func) {
            self.func_declaration(FuncContext::Func)
        } else if match_token_type!(self, TokT::Struct) {
            self.struct_declaration()
        } else if match_token_type!(self, TokT::Impl) {
            self.impl_block()
        } else {
            self.statement()
        }
    }
    fn let_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if !match_token_type!(self, TokT::Identifier) {
            Err(self.error_current("Expected variable field."))
        } else {
            let name = self.previous_token.clone();
            let type_info = if match_token_type!(self, TokT::Colon) {
                self.type_block()?
            } else {
                TypeAst::Infer // The type will be inferred by the compiler later on.
            };

            self.consume(TokT::Equal, "Expected '=' after variable field.")?;
            let expr = self.expression()?;
            self.consume(TokT::Semicolon, "Expected ';' after variable declaration.")?;
            Ok(Stmt::Let {
                identifier: name,
                value: expr,
                type_info,
            })
        }
    }
    fn func_declaration(
        &mut self,
        func_context: FuncContext<'src>,
    ) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(
            TokT::Identifier,
            "Expected function name. Syntax: 'func name(params) : return_type { body }",
        )?;

        let name = self.previous_token.clone();
        self.consume(TokT::LeftParen, "Expected '(' after function field.")?;
        let mut params = vec![];
        let mut param_types = vec![];

        if !check_token_type!(self, TokT::RightParen) {
            loop {
                if let FuncContext::Method(type_name) = &func_context
                    && match_token_type!(self, TokT::Self_)
                {
                    params.push(self.previous_token.clone());
                    param_types.push(TypeAst::Named(Token {
                        token_type: TokenType::Identifier,
                        line: self.previous_token.line,
                        lexeme: type_name.lexeme,
                    }));
                } else {
                    self.consume(
                        TokT::Identifier,
                        "Expected the field of the function parameter.",
                    )?;
                    params.push(self.previous_token.clone());
                    self.consume(
                        TokT::Colon,
                        "Expected ':' after parameter field. Syntax: 'field: type",
                    )?;
                    param_types.push(self.type_block()?);
                }

                if !match_token_type!(self, TokT::Comma) {
                    break;
                }
            }
        }

        self.consume(TokT::RightParen, "Expected ')' after parameters.")?;

        let return_type = if match_token_type!(self, TokT::Colon) {
            self.type_block()?
        } else {
            TypeAst::Named(Token::new(
                TokenType::Identifier,
                self.previous_token.line,
                "void",
            ))
        };

        self.consume(TokT::LeftBrace, "Expected '{' before function body.")?;
        let mut body = vec![];

        while !match_token_type!(self, TokT::RightBrace) {
            body.push(self.declaration()?);
        }

        Ok(Stmt::Function {
            type_: TypeAst::Function {
                param_types: param_types.into_boxed_slice(),
                return_type: Box::new(return_type),
            },
            name,
            params,
            body,
        })
    }
    fn type_block(&mut self) -> Result<TypeAst<'src>, ParserError<'src>> {
        match self.current_token.token_type {
            TokT::Func => {
                self.consume(TokT::Func, "Expected 'func' keyword.")?;
                self.consume(
                    TokT::LeftParen,
                    "Expected 'paren' after func type definition",
                )?;
                let mut argument_types = vec![];

                if !check_token_type!(self, TokT::RightParen) {
                    loop {
                        let arg = self.type_block()?;
                        argument_types.push(arg);

                        if !match_token_type!(self, TokT::Comma) {
                            break;
                        }
                    }
                }
                self.consume(
                    TokT::RightParen,
                    "Expected ')' after function type parameters.",
                )?;

                let return_type = if match_token_type!(self, TokT::Colon) {
                    self.type_block()?
                } else {
                    TypeAst::Named(Token::new(
                        TokenType::Identifier,
                        self.previous_token.line,
                        "void",
                    ))
                };
                Ok(TypeAst::Function {
                    param_types: Box::from(argument_types),
                    return_type: Box::new(return_type),
                })
            }
            TokT::Identifier => {
                self.consume(
                    TokT::Identifier,
                    "Expected the field of the return type of the function.",
                )?;

                Ok(TypeAst::Named(self.previous_token.clone()))
            }
            _ => Err(self.error_current("Expected the field of the return type of the function.")),
        }
    }

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
        let brace_token = self.previous_token.clone();
        let mut statements = vec![];
        while !check_token_type!(self, TokT::RightBrace) {
            statements.push(self.declaration()?);
        }

        self.consume(TokT::RightBrace, "Expected '}' after block.")?;
        Ok(Stmt::Block {
            body: statements,
            brace_token,
        })
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

    fn struct_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::Identifier, "Expected struct name.")?;
        let name = self.previous_token.clone();
        self.consume(TokT::LeftBrace, "Expected '{' before struct body.")?;
        let mut fields = vec![];

        while !check_token_type!(self, TokT::RightBrace) {
            self.consume(TokT::Identifier, "Expected field name")?;
            let field_name = self.previous_token.clone();
            self.consume(TokT::Colon, "Expected ':' after field name")?;
            let field_type = self.type_block()?;
            fields.push((field_name, field_type));
            match_token_type!(self, TokT::Comma); // Optional trailing comma.
        }
        self.consume(TokT::RightBrace, "Expected '}' after struct body.")?;
        Ok(Stmt::Struct { name, fields })
    }

    fn impl_block(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::Identifier, "Expected name of type.")?;
        let name = self.previous_token.clone();
        self.consume(TokT::LeftBrace, "Expected '{' before impl block.")?;
        let mut methods = vec![];
        while match_token_type!(self, TokT::Func) {
            let method = self.func_declaration(FuncContext::Method(name.clone()))?;
            methods.push(method);
        }
        self.consume(TokT::RightBrace, "Expected '}' after impl block.")?;
        Ok(Stmt::Impl { name, methods })
    }
}
