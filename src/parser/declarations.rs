//! Declarations (`let`, `func`, `extern func`, `struct`, `enum`, `impl`, `interface`), their shared pieces
//! (bindings, signatures, generics, annotations) and type syntax.

use crate::parser::ast::{
    Binding, FunctionSig, MethodSig, Stmt, StructField, TypeAst, VariantType,
};
use crate::parser::error::ParserError;
use crate::parser::lists::{Delim, Delimited, ListSpec};
use crate::parser::{PResult, Parser, TokT};
use crate::scanner::{Span, Token, TokenType};

type FunctionDecl<'src> = (Token<'src>, Vec<Token<'src>>, FunctionSig<'src>);
type Param<'src> = (Token<'src>, TypeAst<'src>);

/// FIRST set of a type: the tokens `type_block` dispatches on.
pub(super) const TYPE_START: &[TokT] = &[TokT::Identifier, TokT::LeftParen, TokT::Func];

/// FIRST set of a binding: the tokens `parse_binding` accepts.
pub(super) const BINDING_START: &[TokT] = &[TokT::Identifier, TokT::LeftParen];

/// `(key: binding, :name)` after a struct or variant name. Shared by `let` destructuring and match patterns.
pub(super) const STRUCT_DESTRUCTURE: ListSpec =
    ListSpec::paren("field binding", &[TokT::Identifier, TokT::Colon]);

/// The `void` return type of a signature without `: T`, placed at `span`.
fn void_at(span: Span) -> TypeAst<'static> {
    TypeAst::Named(Token::new(TokenType::Identifier, span, "void"), vec![])
}

/// `public` or nothing, parsed wherever it may appear; the construct decides whether it's allowed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Visibility<'src> {
    Private,
    Public(Token<'src>),
}

impl Visibility<'_> {
    pub(crate) fn is_public(self) -> bool {
        matches!(self, Visibility::Public(_))
    }
}

impl<'src> Parser<'src> {
    pub(super) fn let_declaration(&mut self, vis: Visibility<'src>) -> PResult<'src, Stmt<'src>> {
        self.advance_kind(TokT::Let);
        let binding = self.parse_binding("'let'")?;
        let type_info = self.type_annotation()?.unwrap_or(TypeAst::Infer);
        self.expect(TokT::Equal, "variable binding")?;
        let value = self.expression()?;
        self.expect_semi("variable declaration")?;
        Ok(Stmt::Let {
            is_public: vis.is_public(),
            binding,
            value,
            type_info,
        })
    }

    /// `after` names what precedes the binding, for the "expected a name" error.
    pub(super) fn parse_binding(&mut self, after: &'static str) -> PResult<'src, Binding<'src>> {
        match self.current.kind {
            TokT::LeftParen => {
                let fields = self.delimited(ListSpec::paren("binding", BINDING_START), |p| {
                    p.parse_binding("'('")
                });
                self.validate_tuple(&fields, "tuple binding");
                Ok(Binding::Tuple {
                    fields: fields.items,
                })
            }
            TokT::Identifier => {
                let name = self.advance();
                if self.check(TokT::LeftParen) {
                    let fields = self.delimited(STRUCT_DESTRUCTURE, Self::destructure_field);
                    Ok(Binding::Struct {
                        name,
                        fields: fields.items,
                    })
                } else {
                    Ok(Binding::Variable(name))
                }
            }
            // Always an error here; reports keywords used as names specifically.
            _ => self.expect_ident(after).map(Binding::Variable),
        }
    }

    /// `key: binding`, or `: name` as shorthand for `name: name`.
    pub(super) fn destructure_field(&mut self) -> PResult<'src, (Token<'src>, Binding<'src>)> {
        let key = if self.check(TokT::Identifier) {
            Some(self.advance())
        } else {
            None
        };
        self.expect(TokT::Colon, "field name")?;
        let value = self.parse_binding("':'")?;
        match (key, value) {
            (Some(key), value) => Ok((key, value)),
            (None, Binding::Variable(name)) => Ok((name, Binding::Variable(name))),
            (None, value) => Err(ParserError::Invalid {
                span: value.span(),
                message: "Expected variable name after ':'.\n\
                          Either provide explicit key to the left or provide variable binding to the right."
                    .to_string(),
            }),
        }
    }

    /// Parenthesised lists that are tuples: `()` is an error, and one item needs a trailing comma (`(x,)`), since
    /// `(x)` would be a grouping.
    pub(super) fn validate_tuple<T>(&mut self, list: &Delimited<T>, what: &'static str) {
        if list.failed {
            return;
        }
        let message = match (list.items.len(), list.trailing) {
            (0, _) => format!("Empty {what}: a tuple needs at least one element."),
            (1, None) => format!("A one-element {what} needs a trailing comma: `(x,)`."),
            _ => return,
        };
        self.errors.push(ParserError::Invalid {
            span: list.open.to(list.close),
            message,
        });
    }

    /// `: T` if present (`let`, parameters, lambda parameters).
    pub(super) fn type_annotation(&mut self) -> PResult<'src, Option<TypeAst<'src>>> {
        if self.try_consume(TokT::Colon) {
            self.type_block().map(Some)
        } else {
            Ok(None)
        }
    }

    /// `: T` after a parameter list, if present (functions, function types, lambdas).
    pub(super) fn return_type(&mut self) -> PResult<'src, Option<TypeAst<'src>>> {
        self.type_annotation()
    }

    /// `<T, U>` if the current token is `<`.
    fn generic_params(&mut self) -> Option<Delimited<Token<'src>>> {
        self.check(TokT::Less).then(|| {
            self.delimited(
                ListSpec::angle("generic parameter", &[TokT::Identifier]),
                |p| p.expect_ident("'<'"),
            )
        })
    }

    /// `<A, B>` if the current token is `<`.
    pub(super) fn generic_args(&mut self) -> Option<Delimited<TypeAst<'src>>> {
        self.check(TokT::Less).then(|| {
            self.delimited(
                ListSpec::angle("type argument", TYPE_START),
                Self::type_block,
            )
        })
    }

    fn generic_param_list(&mut self) -> Vec<Token<'src>> {
        self.generic_params().map_or_else(Vec::new, |g| g.items)
    }

    pub(super) fn generic_arg_list(&mut self) -> Vec<TypeAst<'src>> {
        self.generic_args().map_or_else(Vec::new, |g| g.items)
    }

    /// `in_impl`: whether `self` parameters are allowed.
    pub(super) fn func_declaration(
        &mut self,
        vis: Visibility<'src>,
        in_impl: bool,
    ) -> PResult<'src, Stmt<'src>> {
        self.advance_kind(TokT::Func);
        let (name, generics, signature) = self.func_signature()?;
        self.validate_params(&signature.params, in_impl);
        let body = self.block(false, "function signature")?;

        Ok(Stmt::Function {
            is_public: vis.is_public(),
            generics,
            signature,
            name,
            body: Box::new(body.into_stmt()),
        })
    }

    pub(super) fn visibility(&mut self) -> Visibility<'src> {
        if self.try_consume(TokT::Public) {
            Visibility::Public(self.previous)
        } else {
            Visibility::Private
        }
    }

    /// Reports `public` on a construct that doesn't allow it. Parsing continues as if it weren't there.
    pub(super) fn forbid_visibility(&mut self, vis: Visibility<'src>, reason: &'static str) {
        if let Visibility::Public(token) = vis {
            self.errors.push(ParserError::ParseError {
                token,
                message: format!("`public` is not allowed here: {reason}"),
            });
        }
    }

    pub(super) fn extern_func_declaration(
        &mut self,
        vis: Visibility<'src>,
        in_impl: bool,
    ) -> PResult<'src, Stmt<'src>> {
        self.advance_kind(TokT::Extern);
        self.expect(TokT::Func, "'extern'")?;
        let (name, generics, signature) = self.func_signature()?;
        self.validate_params(&signature.params, in_impl);
        self.expect_semi("extern function signature")?;
        Ok(Stmt::ExternFunction {
            is_public: vis.is_public(),
            name,
            generics,
            signature,
        })
    }

    /// `name<G>(params): T`, after `func`. Accepts `self` anywhere; [`Self::validate_params`] decides.
    fn func_signature(&mut self) -> PResult<'src, FunctionDecl<'src>> {
        let name = self.expect_ident("'func'")?;
        let generics = self.generic_param_list();
        let open = self.expect(TokT::LeftParen, "function name")?.span;
        let params = self.delimited_after(
            open,
            ListSpec::paren("parameter", &[TokT::Identifier, TokT::Self_]),
            Self::param,
        );
        let return_type = self.return_type()?.unwrap_or_else(|| void_at(params.close));
        Ok((
            name,
            generics,
            FunctionSig {
                params: params.items.into_boxed_slice(),
                return_type: Box::new(return_type),
            },
        ))
    }

    fn param(&mut self) -> PResult<'src, Param<'src>> {
        if self.check(TokT::Self_) {
            let param = self.advance();
            let ty = TypeAst::Named(
                Token::new(TokenType::Identifier, param.span, "Self"),
                vec![],
            );
            return Ok((param, ty));
        }
        let name = self.expect_ident("'('")?;
        self.expect(TokT::Colon, "parameter name")?;
        let ty = self.type_block()?;
        Ok((name, ty))
    }

    /// `self` is only allowed in methods, as the first parameter. A later `self` (including a duplicate) is
    /// reported on its own token.
    fn validate_params(&mut self, params: &[Param<'src>], allow_self: bool) {
        for (i, (token, _)) in params.iter().enumerate() {
            if token.kind != TokT::Self_ {
                continue;
            }
            let message = if !allow_self {
                "`self` is only allowed in methods."
            } else if i != 0 {
                "`self` must be the first parameter."
            } else {
                continue;
            };
            self.errors.push(ParserError::ParseError {
                token: *token,
                message: message.to_string(),
            });
        }
    }

    pub(super) fn type_block(&mut self) -> PResult<'src, TypeAst<'src>> {
        let ty = match self.current.kind {
            TokT::Func => return self.function_type(),
            TokT::LeftParen => {
                let types = self.delimited(ListSpec::paren("type", TYPE_START), Self::type_block);
                self.validate_tuple(&types, "tuple type");
                TypeAst::Tuple(types.items)
            }
            TokT::Identifier => {
                let name = self.advance();
                TypeAst::Named(name, self.generic_arg_list())
            }
            _ => {
                return Err(ParserError::ParseError {
                    token: self.current,
                    message: format!("Expected a type, found '{}'.", self.current.lexeme),
                });
            }
        };
        if self.try_consume(TokT::Question) {
            Ok(TypeAst::Optional(Box::new(ty)))
        } else {
            Ok(ty)
        }
    }

    /// `func(T, U): R`, or `func?(T): R` for an optional function.
    fn function_type(&mut self) -> PResult<'src, TypeAst<'src>> {
        let keyword = self.advance_kind(TokT::Func).span;
        let optional = self.check(TokT::QuestionParen);
        let open = if optional {
            self.advance().span
        } else {
            self.expect(TokT::LeftParen, "'func'")?.span
        };
        let params =
            self.delimited_after(open, ListSpec::paren("parameter type", TYPE_START), |p| {
                let ty = p.type_block()?;
                // Function types have no parameter names; the AST still wants one (Phase 4 removes it).
                Ok((Token::new(TokenType::Identifier, ty.span(), "_"), ty))
            });
        let return_type = self.return_type()?.unwrap_or_else(|| void_at(params.close));
        let func_type = TypeAst::Function(
            keyword,
            FunctionSig {
                params: params.items.into_boxed_slice(),
                return_type: Box::new(return_type),
            },
        );
        if optional {
            Ok(TypeAst::Optional(Box::new(func_type)))
        } else {
            Ok(func_type)
        }
    }

    pub(super) fn struct_declaration(
        &mut self,
        vis: Visibility<'src>,
    ) -> PResult<'src, Stmt<'src>> {
        self.advance_kind(TokT::Struct);
        let name = self.expect_ident("'struct'")?;
        let generics = self.generic_param_list();
        let open = self.expect(TokT::LeftBrace, "struct name")?.span;
        let fields = self.delimited_after(
            open,
            ListSpec::brace("field", &[TokT::Identifier, TokT::Public]),
            |p| {
                let vis = p.visibility();
                p.struct_field(vis)
            },
        );
        Ok(Stmt::Struct {
            is_public: vis.is_public(),
            name,
            fields: fields.items,
            generics,
        })
    }

    fn struct_field(&mut self, vis: Visibility<'src>) -> PResult<'src, StructField<'src>> {
        let name = self.expect_ident("'{'")?;
        self.expect(TokT::Colon, "field name")?;
        let type_info = self.type_block()?;
        Ok(StructField {
            is_public: vis.is_public(),
            name,
            type_info,
        })
    }

    pub(super) fn enum_declaration(&mut self, vis: Visibility<'src>) -> PResult<'src, Stmt<'src>> {
        self.advance_kind(TokT::Enum);
        let name = self.expect_ident("'enum'")?;
        let generics = self.generic_param_list();
        let open = self.expect(TokT::LeftBrace, "enum name")?.span;
        let variants = self.delimited_after(
            open,
            ListSpec::brace("variant", &[TokT::Identifier, TokT::Public]),
            Self::enum_variant,
        );
        Ok(Stmt::Enum {
            is_public: vis.is_public(),
            name,
            variants: variants.items,
            generics,
        })
    }

    fn enum_variant(&mut self) -> PResult<'src, (Token<'src>, VariantType<'src>)> {
        let vis = self.visibility();
        self.forbid_visibility(vis, "enum variants are always as visible as the enum.");
        let name = self.expect_ident("'{'")?;
        let payload = match self.current.kind {
            TokT::LeftParen => {
                let types = self.delimited(
                    ListSpec::paren("payload type", TYPE_START),
                    Self::type_block,
                );
                if types.items.is_empty() && !types.failed {
                    self.errors.push(ParserError::Invalid {
                        span: types.open.to(types.close),
                        message: "Empty variant payload: remove the `()`.".to_string(),
                    });
                }
                VariantType::Tuple(types.items)
            }
            TokT::LeftBrace => {
                let fields = self.delimited(
                    ListSpec::brace("field", &[TokT::Identifier, TokT::Public]),
                    |p| {
                        let vis = p.visibility();
                        p.forbid_visibility(
                            vis,
                            "enum variant fields are always as visible as the enum.",
                        );
                        p.struct_field(Visibility::Private)
                    },
                );
                VariantType::Struct(fields.items)
            }
            _ => VariantType::Unit,
        };
        Ok((name, payload))
    }

    pub(super) fn impl_block(&mut self) -> PResult<'src, Stmt<'src>> {
        self.advance_kind(TokT::Impl);
        let generics = self.generic_param_list();
        let name = self.expect_ident("'impl'")?;
        let target_generics = self.generic_arg_list();

        let mut interfaces = vec![];
        if self.try_consume(TokT::Colon) {
            interfaces = self.separated(
                &[TokT::LeftBrace],
                "interface name",
                &[TokT::Identifier],
                |p| p.expect_ident("':'"),
            );
            if interfaces.is_empty() {
                return Err(self.err_expected(TokT::Identifier, "':'"));
            }
        }

        let open = self.expect(TokT::LeftBrace, "impl header")?.span;
        let methods = self.sequence(&[TokT::RightBrace], Self::recover_item, Self::impl_item);
        self.close_delimiter(open, Delim::Brace);

        // TODO support generic interfaces
        Ok(Stmt::Impl {
            generics,
            name: (name, target_generics),
            interfaces,
            methods,
        })
    }

    fn impl_item(&mut self) -> PResult<'src, Stmt<'src>> {
        let vis = self.visibility();
        match self.current.kind {
            TokT::Func => self.func_declaration(vis, true),
            TokT::Extern => self.extern_func_declaration(vis, true),
            _ => Err(self.err_expected(TokT::Func, "'{' in impl body")),
        }
    }

    pub(super) fn interface_declaration(
        &mut self,
        vis: Visibility<'src>,
    ) -> PResult<'src, Stmt<'src>> {
        self.advance_kind(TokT::Interface);
        let name = self.expect_ident("'interface'")?;
        let generics = self.generic_param_list();
        let open = self.expect(TokT::LeftBrace, "interface name")?.span;
        let methods = self.sequence(&[TokT::RightBrace], Self::recover_item, |p| {
            let vis = p.visibility();
            p.forbid_visibility(
                vis,
                "interface methods are always as visible as the interface.",
            );
            p.interface_method_sig()
        });
        self.close_delimiter(open, Delim::Brace);
        Ok(Stmt::Interface {
            is_public: vis.is_public(),
            name,
            methods,
            generics,
        })
    }

    fn interface_method_sig(&mut self) -> PResult<'src, MethodSig<'src>> {
        self.expect(TokT::Func, "'{' in interface body")?;
        let (name, generics, signature) = self.func_signature()?;
        self.validate_params(&signature.params, true);
        self.expect_semi("interface method signature")?;
        Ok(MethodSig {
            generics,
            name,
            signature,
        })
    }
}
