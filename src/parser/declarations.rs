use crate::parser::ast::{Binding, FunctionSig, MethodSig, Stmt, TypeAst, VariantType};
use crate::parser::error::ParserError;
use crate::parser::{check_token_type, match_token_type, Parser, TokT};
use crate::scanner::{Span, Token, TokenType};

type FunctionDecl<'src> = (Token<'src>, Vec<Token<'src>>, FunctionSig<'src>);

impl<'src> Parser<'src> {
    pub(super) fn let_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
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

    pub(super) fn parse_binding(&mut self) -> Result<Binding<'src>, ParserError<'src>> {
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

    pub(super) fn parse_destructure(
        &mut self,
        name: Token<'src>,
    ) -> Result<Binding<'src>, ParserError<'src>> {
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
                        return Err(self.error_previous(
                            "Expected variable name after ':'.\n\
                             Either provide explicit key to the left or provide variable binding to the right.",
                        ));
                    }
                }
                Some(key) => fields.push((key, val)),
            }
            match_token_type!(self, TokT::Comma);
        }
        Ok(Binding::Struct { name, fields })
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

    pub(super) fn func_declaration(
        &mut self,
        is_method: bool,
    ) -> Result<Stmt<'src>, ParserError<'src>> {
        let (name, generics, signature) = self.func_signature(is_method)?;
        let body = self.block()?;

        Ok(Stmt::Function {
            generics,
            signature,
            name,
            body: Box::new(body),
        })
    }

    pub(super) fn extern_func_declaration(
        &mut self,
        is_method: bool,
    ) -> Result<Stmt<'src>, ParserError<'src>> {
        let (name, generics, signature) = self.func_signature(is_method)?;
        self.consume(
            TokT::Semicolon,
            "Expected ';' after extern function signature.",
        )?;
        Ok(Stmt::ExternFunction {
            name,
            generics,
            signature,
        })
    }

    fn func_signature(&mut self, is_method: bool) -> Result<FunctionDecl<'src>, ParserError<'src>> {
        self.consume(
            TokT::Identifier,
            "Expected method name. Syntax: 'func name(self, ...): type;'",
        )?;
        let name = self.previous_token.clone();

        let generics = self.parse_generic_params()?;
        self.consume(TokT::LeftParen, "Expected '(' after function name.")?;
        let mut params = vec![];

        if !check_token_type!(self, TokT::RightParen) {
            loop {
                if is_method && match_token_type!(self, TokT::Self_) {
                    let param = self.previous_token.clone();
                    let ty = TypeAst::Named(
                        Token {
                            token_type: TokenType::Identifier,
                            span: self.previous_token.span,
                            lexeme: "Self",
                        },
                        vec![],
                    );
                    params.push((param, ty));
                } else {
                    self.consume(TokT::Identifier, "Expected parameter name.")?;
                    let param = self.previous_token.clone();
                    self.consume(TokT::Colon, "Expected ':' after parameter name.")?;
                    let ty = self.type_block()?;
                    params.push((param, ty));
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
                Token::new(TokenType::Identifier, self.previous_token.span, "void"),
                vec![],
            )
        };
        Ok((
            name,
            generics,
            FunctionSig {
                params: Box::from(params),
                return_type: Box::new(return_type),
            },
        ))
    }

    pub(super) fn type_block(&mut self) -> Result<TypeAst<'src>, ParserError<'src>> {
        const UNDER: Token<'static> = Token {
            token_type: TokenType::Identifier,
            span: Span::new(0, 0, 0),
            lexeme: "_",
        };

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
                let mut params = vec![];

                if !check_token_type!(self, TokT::RightParen) {
                    loop {
                        let arg = self.type_block()?;
                        params.push((UNDER, arg));

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
                        Token::new(TokenType::Identifier, self.previous_token.span, "void"),
                        vec![],
                    )
                };
                let func_type = TypeAst::Function(FunctionSig {
                    params: Box::from(params),
                    return_type: Box::new(return_type),
                });
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
                    return Err(self.error_previous("Tuple types must have at least 2 fields."));
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

    pub(super) fn struct_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
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
            // TODO review trailing commas
            match_token_type!(self, TokT::Comma); // Optional trailing comma.
        }
        self.consume(TokT::RightBrace, "Expected '}' after struct body.")?;
        Ok(Stmt::Struct {
            name,
            fields,
            generics,
        })
    }

    pub(super) fn enum_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
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

    pub(super) fn impl_block(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
        let generics = self.parse_generic_params()?;

        self.consume(TokT::Identifier, "Expected name of type.")?;
        let name = self.previous_token.clone();

        let mut target_generics = vec![];
        if match_token_type!(self, TokT::Less) {
            loop {
                let ty = self.type_block()?;
                target_generics.push(ty);
                if match_token_type!(self, TokT::Comma) {
                    continue;
                }
                break;
            }
            self.consume(TokT::Greater, "Expected '>' after generic parameters.")?;
        }

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
        while check_token_type!(self, TokT::Func) || check_token_type!(self, TokT::Extern) {
            if match_token_type!(self, TokT::Extern) {
                self.consume(TokT::Func, "Expected 'func' after 'extern'.")?;
                methods.push(self.extern_func_declaration(true)?);
            } else {
                match_token_type!(self, TokT::Func);
                methods.push(self.func_declaration(true)?);
            }
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

    pub(super) fn interface_declaration(&mut self) -> Result<Stmt<'src>, ParserError<'src>> {
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

    fn interface_method_sig(&mut self) -> Result<MethodSig<'src>, ParserError<'src>> {
        let (name, generics, signature) = self.func_signature(true)?;

        self.consume(
            TokT::Semicolon,
            "Expected ';' after interface method signature.",
        )?;
        Ok(MethodSig {
            generics,
            name,
            signature,
        })
    }
}
