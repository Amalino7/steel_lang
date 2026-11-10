//! Declarations (`let`, `func`, `extern func`, `struct`, `enum`, `impl`, `interface`).

use crate::parser::ast::{
    EnumDecl, FieldDecl, FnDecl, FnSig, ImplDecl, InterfaceDecl, LetDecl, MethodSig, Param,
    StructDecl, Variant, VariantPayload, Visibility,
};
use crate::parser::error::{Expected, ParenContext, ParseErrorKind};
use crate::parser::lists::{Delim, ListSpec};
use crate::parser::statements::StmtCtx;
use crate::parser::types::TYPE_START;
use crate::parser::{PResult, Parser, TokT};
use crate::scanner::Span;

/// Where a function declaration appears, which decides whether `self` parameters are allowed.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(super) enum FnContext {
    Free,
    Method,
}

impl<'src> Parser<'src> {
    /// `let binding: T = value;`, with `vis` already parsed.
    pub(super) fn let_declaration(
        &mut self,
        vis: Visibility,
        start: Span,
    ) -> PResult<LetDecl<'src>> {
        self.advance_kind(TokT::Let);
        let binding = self.binding("`let`")?;
        let ty = self.type_annotation()?;
        self.expect(TokT::Equal, "variable binding")?;
        let value = self.expression()?;
        self.expect_semi("variable declaration")?;
        Ok(LetDecl {
            vis,
            binding,
            ty,
            value,
            span: self.span_from(start),
        })
    }

    pub(super) fn visibility(&mut self) -> Visibility {
        if self.try_consume(TokT::Public) {
            Visibility::Public(self.previous.span)
        } else {
            Visibility::Private
        }
    }

    /// Reports `public` on a construct that doesn't allow it. Parsing continues as if it weren't there.
    pub(super) fn forbid_visibility(&mut self, vis: Visibility, reason: &'static str) {
        if let Visibility::Public(span) = vis {
            self.report(ParseErrorKind::VisibilityNotAllowed { reason }, span);
        }
    }

    /// `func sig { body }`, with `vis` already parsed.
    pub(super) fn func_declaration(
        &mut self,
        vis: Visibility,
        start: Span,
        context: FnContext,
    ) -> PResult<FnDecl<'src>> {
        self.advance_kind(TokT::Func);
        let sig = self.func_signature(context)?;
        let body = self.block(StmtCtx::Body, "function signature")?.into_body();
        Ok(FnDecl {
            vis,
            sig,
            body: Some(body),
            span: self.span_from(start),
        })
    }

    /// `extern func sig;`, with `vis` already parsed.
    pub(super) fn extern_func_declaration(
        &mut self,
        vis: Visibility,
        start: Span,
        context: FnContext,
    ) -> PResult<FnDecl<'src>> {
        self.advance_kind(TokT::Extern);
        self.expect(TokT::Func, "`extern`")?;
        let sig = self.func_signature(context)?;
        self.expect_semi("extern function signature")?;
        Ok(FnDecl {
            vis,
            sig,
            body: None,
            span: self.span_from(start),
        })
    }

    /// `name<G>(params): T`, after `func`. Accepts `self` anywhere and reports it where `context` doesn't allow it.
    fn func_signature(&mut self, context: FnContext) -> PResult<FnSig<'src>> {
        let name = self.expect_ident("`func`")?;
        let generics = self.generic_params();
        let open = self.expect(TokT::LeftParen, "function name")?.span;
        let params = self.delimited_after(
            open,
            ListSpec::paren("parameter", &[TokT::Identifier, TokT::Self_]),
            Self::param,
        );
        self.validate_params(&params.items, context);
        let ret = self.type_annotation()?;
        Ok(FnSig {
            name,
            generics,
            params: params.into_ast(),
            ret,
        })
    }

    fn param(&mut self) -> PResult<Param<'src>> {
        if self.check(TokT::Self_) {
            return Ok(Param::SelfParam(self.advance().span));
        }
        let name = self.expect_ident("`(`")?;
        self.expect(TokT::Colon, "parameter name")?;
        let ty = self.type_ast()?;
        Ok(Param::Named { name, ty })
    }

    /// `self` is only allowed in methods, as the first parameter. A later `self` (including a duplicate) is
    /// reported on its own token.
    fn validate_params(&mut self, params: &[Param<'src>], context: FnContext) {
        for (i, param) in params.iter().enumerate() {
            let Param::SelfParam(span) = param else {
                continue;
            };
            let kind = if context != FnContext::Method {
                ParseErrorKind::SelfOutsideMethod
            } else if i != 0 {
                ParseErrorKind::SelfNotFirst {
                    first: params[0].span(),
                    duplicate: matches!(params[0], Param::SelfParam(_)),
                }
            } else {
                continue;
            };
            self.report(kind, *span);
        }
    }

    pub(super) fn struct_declaration(
        &mut self,
        vis: Visibility,
        start: Span,
    ) -> PResult<StructDecl<'src>> {
        self.advance_kind(TokT::Struct);
        let name = self.expect_ident("`struct`")?;
        let generics = self.generic_params();
        let open = self.expect(TokT::LeftBrace, "struct name")?.span;
        let fields = self.delimited_after(
            open,
            ListSpec::brace("field", &[TokT::Identifier, TokT::Public]),
            |p| {
                let start = p.start_span();
                let vis = p.visibility();
                p.field_decl(vis, start)
            },
        );
        Ok(StructDecl {
            vis,
            name,
            generics,
            fields: fields.items,
            span: self.span_from(start),
        })
    }

    fn field_decl(&mut self, vis: Visibility, start: Span) -> PResult<FieldDecl<'src>> {
        let name = self.expect_ident("`{`")?;
        self.expect(TokT::Colon, "field name")?;
        let ty = self.type_ast()?;
        Ok(FieldDecl {
            vis,
            name,
            ty,
            span: self.span_from(start),
        })
    }

    pub(super) fn enum_declaration(
        &mut self,
        vis: Visibility,
        start: Span,
    ) -> PResult<EnumDecl<'src>> {
        self.advance_kind(TokT::Enum);
        let name = self.expect_ident("`enum`")?;
        let generics = self.generic_params();
        let open = self.expect(TokT::LeftBrace, "enum name")?.span;
        let variants = self.delimited_after(
            open,
            ListSpec::brace("variant", &[TokT::Identifier, TokT::Public]),
            Self::enum_variant,
        );
        Ok(EnumDecl {
            vis,
            name,
            generics,
            variants: variants.items,
            span: self.span_from(start),
        })
    }

    fn enum_variant(&mut self) -> PResult<Variant<'src>> {
        let start = self.start_span();
        let vis = self.visibility();
        self.forbid_visibility(vis, "enum variants are always as visible as the enum.");
        let name = self.expect_ident("`{`")?;
        let payload = match self.current.kind {
            TokT::LeftParen => {
                let types =
                    self.delimited(ListSpec::paren("payload type", TYPE_START), Self::type_ast);
                if types.items.is_empty() && !types.failed {
                    self.report(
                        ParseErrorKind::EmptyParens(ParenContext::VariantPayload),
                        types.span(),
                    );
                }
                VariantPayload::Tuple(types.items)
            }
            TokT::LeftBrace => {
                let fields = self.delimited(
                    ListSpec::brace("field", &[TokT::Identifier, TokT::Public]),
                    |p| {
                        let start = p.start_span();
                        let vis = p.visibility();
                        p.forbid_visibility(
                            vis,
                            "enum variant fields are always as visible as the enum.",
                        );
                        p.field_decl(Visibility::Private, start)
                    },
                );
                VariantPayload::Struct(fields.items)
            }
            _ => VariantPayload::Unit,
        };
        Ok(Variant {
            name,
            payload,
            span: self.span_from(start),
        })
    }

    pub(super) fn impl_block(&mut self, start: Span) -> PResult<ImplDecl<'src>> {
        self.advance_kind(TokT::Impl);
        let generics = self.generic_params();
        let target = self.expect_named_type("`impl`")?;

        let mut interfaces = vec![];
        if self.try_consume(TokT::Colon) {
            interfaces = self.separated(
                &[TokT::LeftBrace],
                "interface name",
                &[TokT::Identifier],
                |p| p.expect_named_type("`:`"),
            );
            if interfaces.is_empty() {
                return Err(self.err_expected(TokT::Identifier, "`:`"));
            }
        }

        let open = self.expect(TokT::LeftBrace, "impl header")?.span;
        let methods = self.sequence(&[TokT::RightBrace], Self::recover_item, Self::impl_item);
        self.close_delimiter(open, Delim::Brace);

        Ok(ImplDecl {
            generics,
            target,
            interfaces,
            methods,
            span: self.span_from(start),
        })
    }

    fn impl_item(&mut self) -> PResult<FnDecl<'src>> {
        let start = self.start_span();
        let vis = self.visibility();
        match self.current.kind {
            TokT::Func => self.func_declaration(vis, start, FnContext::Method),
            TokT::Extern => self.extern_func_declaration(vis, start, FnContext::Method),
            _ => Err(self.err_unexpected(Expected::Item("method"), None)),
        }
    }

    pub(super) fn interface_declaration(
        &mut self,
        vis: Visibility,
        start: Span,
    ) -> PResult<InterfaceDecl<'src>> {
        self.advance_kind(TokT::Interface);
        let name = self.expect_ident("`interface`")?;
        let generics = self.generic_params();
        let open = self.expect(TokT::LeftBrace, "interface name")?.span;
        let methods = self.sequence(&[TokT::RightBrace], Self::recover_item, |p| {
            let start = p.start_span();
            let vis = p.visibility();
            p.forbid_visibility(
                vis,
                "interface methods are always as visible as the interface.",
            );
            p.interface_method_sig(start)
        });
        self.close_delimiter(open, Delim::Brace);
        Ok(InterfaceDecl {
            vis,
            name,
            generics,
            methods,
            span: self.span_from(start),
        })
    }

    fn interface_method_sig(&mut self, start: Span) -> PResult<MethodSig<'src>> {
        if !self.try_consume(TokT::Func) {
            return Err(self.err_unexpected(Expected::Item("method signature"), None));
        }
        let sig = self.func_signature(FnContext::Method)?;
        self.expect_semi("interface method signature")?;
        Ok(MethodSig {
            sig,
            span: self.span_from(start),
        })
    }
}
