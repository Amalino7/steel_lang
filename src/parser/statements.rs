use crate::parser::ast::{
    Binding, Expr, InterfaceSig, Literal, MatchArm, Pattern, Stmt, TypeAst, VariantType,
};
use crate::parser::error::ParserError;
use crate::parser::{check_next_token_type, TokT};
use crate::parser::{check_token_type, match_token_type, Parser};
use crate::token::{Token, TokenType};

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
        } else {
            self.statement()
        }
    }
    fn let_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let binding = self.parse_binding()?;
        let type_info = if match_token_type!(self, TokT::Colon) {
            self.type_block()?
        } else {
            TypeAst::Infer // The type will be inferred by the compiler later on.
        };

        self.consume(TokT::Equal, "Expected '=' after variable field.")?;
        let expr = self.expression()?;
        self.consume(TokT::Semicolon, "Expected ';' after variable declaration.")?;
        Ok(Stmt::Let {
            binding,
            value: expr,
            type_info,
        })
    }

    fn parse_binding(&mut self) -> Result<Binding<'src>, ParserError<'src>> {
        if match_token_type!(self, TokT::LeftParen) {
            // Tuple
            let mut fields = vec![];
            while !match_token_type!(self, TokT::RightParen) {
                let binding = self.parse_binding()?;
                fields.push(binding);
                match_token_type!(self, TokT::Comma);
            }
            Ok(Binding::Tuple { fields })
        } else if match_token_type!(self, TokT::Identifier) {
            let name = self.previous_token.clone();
            if match_token_type!(self, TokT::LeftParen) {
                // structure
                self.parse_destructure(name)
            } else {
                Ok(Binding::Variable(name))
            }
        } else {
            Err(self.error_current("Expected variable name."))
        }
    }
    fn parse_generic_params(&mut self) -> Result<Vec<Token<'src>>, ParserError<'src>> {
        let mut generics = vec![];
        if match_token_type!(self, TokT::Less) {
            loop {
                self.consume(TokT::Identifier, "Expected generic parameter name.")?;
                generics.push(self.previous_token.clone());

                if match_token_type!(self, TokT::Comma) {
                    continue;
                }
                break;
            }
            self.consume(TokT::Greater, "Expected '>' after generic parameters.")?;
        }
        Ok(generics)
    }

    pub(crate) fn parse_generic_arguments(
        &mut self,
    ) -> Result<Vec<TypeAst<'src>>, ParserError<'src>> {
        let mut args = vec![];
        if match_token_type!(self, TokT::Less) {
            loop {
                args.push(self.type_block()?);
                if match_token_type!(self, TokT::Comma) {
                    continue;
                }
                break;
            }
            self.consume(TokT::Greater, "Expected '>' after generic arguments.")?;
        }
        Ok(args)
    }

    fn parse_destructure(&mut self, name: Token<'src>) -> Result<Binding<'src>, ParserError<'src>> {
        let mut fields = vec![];
        while !match_token_type!(self, TokT::RightParen) {
            let key = if match_token_type!(self, TokT::Identifier) {
                Some(self.previous_token.clone())
            } else {
                None
            };
            self.consume(TokT::Colon, "Expected ':'")?;
            let val = self.parse_binding()?;
            match key {
                None => {
                    if let Binding::Variable(name) = &val {
                        fields.push((name.clone(), val));
                    } else {
                        self.error_previous("Expected variable name after ':'.\n\
                                 Either provide explicit key to the left or provide variable binding to the right.");
                    }
                }
                Some(key) => fields.push((key, val)),
            }
            match_token_type!(self, TokT::Comma);
        }
        Ok(Binding::Struct { name, fields })
    }

    fn func_declaration(&mut self, is_method: bool) -> Result<Stmt<'src>, ParserError<'src>> {
        let (name, generics, params, type_) = self.func_signature(is_method)?;

        self.consume(TokT::LeftBrace, "Expected '{' before function body.")?;
        let mut body = vec![];

        while !match_token_type!(self, TokT::RightBrace) {
            body.push(self.declaration()?);
        }

        Ok(Stmt::Function {
            generics,
            type_,
            name,
            params,
            body,
        })
    }

    fn func_signature(
        &mut self,
        is_method: bool,
    ) -> Result<
        (
            Token<'src>,
            Vec<Token<'src>>,
            Vec<Token<'src>>,
            TypeAst<'src>,
        ),
        ParserError<'src>,
    > {
        self.consume(
            TokT::Identifier,
            "Expected method name. Syntax: 'func name(self, ...): type;'",
        )?;
        let name = self.previous_token.clone();

        let generics = self.parse_generic_params()?;
        self.consume(TokT::LeftParen, "Expected '(' after function name.")?;
        let mut params = vec![];
        let mut param_types = vec![];

        if !check_token_type!(self, TokT::RightParen) {
            loop {
                if is_method && match_token_type!(self, TokT::Self_) {
                    params.push(self.previous_token.clone());
                    param_types.push(TypeAst::Named(
                        Token {
                            token_type: TokenType::Identifier,
                            line: self.previous_token.line,
                            lexeme: "Self",
                        },
                        vec![],
                    ));
                } else {
                    self.consume(TokT::Identifier, "Expected parameter name.")?;
                    params.push(self.previous_token.clone());
                    self.consume(TokT::Colon, "Expected ':' after parameter name.")?;
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
            TypeAst::Named(
                Token::new(TokenType::Identifier, self.previous_token.line, "void"),
                vec![],
            )
        };
        Ok((
            name,
            generics,
            params,
            TypeAst::Function {
                param_types: param_types.into_boxed_slice(),
                return_type: Box::new(return_type),
            },
        ))
    }

    fn type_block(&mut self) -> Result<TypeAst<'src>, ParserError<'src>> {
        match self.current_token.token_type {
            TokT::Func => {
                self.consume(TokT::Func, "Expected 'func' keyword.")?;

                let optional = match_token_type!(self, TokT::QuestionParen);

                if !optional {
                    self.consume(
                        TokT::LeftParen,
                        "Expected 'paren' after func type definition",
                    )?;
                }
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
                    TypeAst::Named(
                        Token::new(TokenType::Identifier, self.previous_token.line, "void"),
                        vec![],
                    )
                };
                let func_type = TypeAst::Function {
                    param_types: Box::from(argument_types),
                    return_type: Box::new(return_type),
                };
                if optional {
                    Ok(TypeAst::Optional(Box::new(func_type)))
                } else {
                    Ok(func_type)
                }
            }
            TokT::LeftParen => {
                self.consume(TokT::LeftParen, "Expected '(' after tuple type definition.")?;
                // Tuple
                let mut types = vec![];
                while !match_token_type!(self, TokT::RightParen) {
                    let type_ = self.type_block()?;
                    types.push(type_);
                    if match_token_type!(self, TokT::Comma) {}
                }
                if types.len() < 2 {
                    self.error_previous("Tuple types must have at least 2 fields.");
                }
                let type_name = TypeAst::Tuple(types);
                if match_token_type!(self, TokT::Question) {
                    Ok(TypeAst::Optional(Box::new(type_name)))
                } else {
                    Ok(type_name)
                }
            }
            TokT::Identifier => {
                self.consume(TokT::Identifier, "Expected the name of the type.")?;
                let name = self.previous_token.clone();
                let generics = self.parse_generic_arguments()?;

                let type_name = TypeAst::Named(name, generics);

                if match_token_type!(self, TokT::Question) {
                    Ok(TypeAst::Optional(Box::new(type_name)))
                } else {
                    Ok(type_name)
                }
            }
            _ => Err(self.error_current("Expected valid type start.")),
        }
    }

    fn statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        if check_token_type!(self, TokT::LeftBrace) {
            self.block()
        } else if match_token_type!(self, TokT::If) {
            self.if_statement()
        } else if match_token_type!(self, TokT::While) {
            self.while_statement()
        } else if match_token_type!(self, TokT::Match) {
            self.match_statement()
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
        let condition = self.expression();
        let condition = condition?;
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
            else_branch: else_branch.map(Box::new),
        })
    }

    fn match_statement(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let expr = self.expression();
        let expr = expr?;

        self.consume(TokT::LeftBrace, "Expected '{' after match statement.")?;
        let mut arms = vec![];
        while !check_token_type!(self, TokT::RightBrace) {
            // pattern
            let pattern = self.pattern()?;
            // =>
            self.consume(TokT::Arrow, "Expected '=>' after match pattern.")?;
            // block
            let block = self.block()?;
            arms.push(MatchArm {
                pattern,
                body: block,
            });
        }
        self.consume(TokT::RightBrace, "Expected '}' after match statement.")?;
        Ok(Stmt::Match {
            value: Box::new(expr),
            arms,
        })
    }

    fn pattern(&mut self) -> Result<Pattern<'src>, ParserError<'src>> {
        if check_token_type!(self, TokT::Identifier) && !check_next_token_type!(self, TokT::Dot) {
            self.consume(TokT::Identifier, "Expected variable name.")?;
            let var_name = self.previous_token.clone();
            return Ok(Pattern::Variable(var_name));
        }

        let enum_name = if match_token_type!(self, TokT::Identifier) {
            Some(self.previous_token.clone())
        } else {
            None
        };
        self.consume(
            TokT::Dot,
            "Expected '.' as pattern start or between enum name and variant ",
        )?;

        self.consume(TokT::Identifier, "Expected variant name.")?;
        let variant_name = self.previous_token.clone();

        let bind = if match_token_type!(self, TokT::LeftParen) {
            // Struct like case, :x or val:val
            if check_token_type!(self, TokT::Colon) || check_next_token_type!(self, TokT::Colon) {
                Some(self.parse_destructure(variant_name.clone())?)
            } else {
                let mut bindings = vec![];
                while !check_token_type!(self, TokT::RightParen) {
                    bindings.push(self.parse_binding()?);
                    match_token_type!(self, TokT::Comma);
                }
                self.consume(TokT::RightParen, "Expected ')' pattern.")?;

                if bindings.is_empty() {
                    None
                } else if bindings.len() == 1 {
                    Some(bindings.remove(0))
                } else {
                    Some(Binding::Tuple { fields: bindings })
                }
            }
        } else {
            None
        };

        Ok(Pattern::Named {
            enum_name,
            variant_name,
            bind,
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

    fn struct_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::Identifier, "Expected struct name.")?;
        let name = self.previous_token.clone();

        let generics = self.parse_generic_params()?;
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
        Ok(Stmt::Struct {
            name,
            fields,
            generics,
        })
    }

    fn enum_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::Identifier, "Expected enum name.")?;
        let name = self.previous_token.clone();
        let generics = self.parse_generic_params()?;
        self.consume(TokT::LeftBrace, "Expected '{' before enum body.")?;

        let mut variants = vec![];
        while !check_token_type!(self, TokT::RightBrace) {
            self.consume(TokT::Identifier, "Expected variant name.")?;
            let variant_name = self.previous_token.clone();

            let variant_type = if match_token_type!(self, TokT::LeftParen) {
                // Tuple Style: Variant(int, int)
                let mut types = vec![];
                while !check_token_type!(self, TokT::RightParen) {
                    types.push(self.type_block()?);
                    if !match_token_type!(self, TokT::Comma) {
                        break;
                    }
                }
                self.consume(TokT::RightParen, "Expected ')' after tuple variant.")?;
                VariantType::Tuple(types)
            } else if match_token_type!(self, TokT::LeftBrace) {
                // Struct Style: Variant { msg: string }
                let mut fields = vec![];
                while !check_token_type!(self, TokT::RightBrace) {
                    self.consume(TokT::Identifier, "Expected field name.")?;
                    let field_name = self.previous_token.clone();
                    self.consume(TokT::Colon, "Expected ':' after field name.")?;
                    let field_type = self.type_block()?;
                    fields.push((field_name, field_type));

                    if !match_token_type!(self, TokT::Comma) {
                        break;
                    }
                }
                self.consume(TokT::RightBrace, "Expected '}' after struct variant.")?;
                VariantType::Struct(fields)
            } else {
                // Unit Style: Variant
                VariantType::Unit
            };

            variants.push((variant_name, variant_type));
            match_token_type!(self, TokT::Comma);
        }
        self.consume(TokT::RightBrace, "Expected '}' after enum body.")?;

        Ok(Stmt::Enum {
            name,
            variants,
            generics,
        })
    }

    fn impl_block(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let generics = self.parse_generic_params()?;

        self.consume(TokT::Identifier, "Expected name of type.")?;
        let name = self.previous_token.clone();
        let target_generics = self.parse_generic_params()?;
        let mut interfaces = vec![];
        if match_token_type!(self, TokT::Colon) {
            loop {
                self.consume(TokT::Identifier, "Expected interface name after ':'.")?;
                interfaces.push(self.previous_token.clone());
                if !match_token_type!(self, TokT::Comma) {
                    break;
                }
            }
        }

        self.consume(TokT::LeftBrace, "Expected '{' before impl block.")?;
        let mut methods = vec![];
        while match_token_type!(self, TokT::Func) {
            let method = self.func_declaration(true)?;
            methods.push(method);
        }
        self.consume(TokT::RightBrace, "Expected '}' after impl block.")?;

        // TODO support generic interfaces
        Ok(Stmt::Impl {
            generics,
            name: (name, target_generics),
            interfaces,
            methods,
        })
    }

    fn interface_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        self.consume(TokT::Identifier, "Expected interface name.")?;
        let name = self.previous_token.clone();
        let generics = self.parse_generic_params()?;
        self.consume(TokT::LeftBrace, "Expected '{' before interface body.")?;
        let mut methods = vec![];

        while !check_token_type!(self, TokT::RightBrace) {
            self.consume(TokT::Func, "Expected 'func' in interface body.")?;
            methods.push(self.interface_method_sig()?);
        }

        self.consume(TokT::RightBrace, "Expected '}' after interface body.")?;
        Ok(Stmt::Interface {
            name,
            methods,
            generics,
        })
    }

    fn interface_method_sig(&mut self) -> Result<InterfaceSig<'src>, ParserError<'src>> {
        let (name, generics, params, type_) = self.func_signature(true)?;

        self.consume(
            TokT::Semicolon,
            "Expected ';' after interface method signature.",
        )?;
        Ok(InterfaceSig {
            generics,
            name,
            params,
            type_,
        })
    }
}
