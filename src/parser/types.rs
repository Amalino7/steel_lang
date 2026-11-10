use crate::parser::TokT;
use crate::parser::ast::{Generics, NamedType, TypeAst, TypeAstKind};
use crate::parser::error::{Expected, ParenContext};
use crate::parser::lists::{ListSpec, ParsedList};
use crate::parser::{PResult, Parser};
use crate::scanner::Span;

/// FIRST set of a type: the tokens `type_ast` dispatches on.
pub(super) const TYPE_START: &[TokT] = &[TokT::Identifier, TokT::LeftParen, TokT::Func];

impl<'src> Parser<'src> {
    /// `: T` if present (`let`, parameters, lambda parameters, return types).
    pub(super) fn type_annotation(&mut self) -> PResult<Option<TypeAst<'src>>> {
        if self.try_consume(TokT::Colon) {
            self.type_ast().map(Some)
        } else {
            Ok(None)
        }
    }

    /// `<T, U>` if the current token is `<`, else nothing.
    pub(super) fn generic_params(&mut self) -> Generics<'src> {
        if !self.check(TokT::Less) {
            return Generics(None);
        }
        let list = self.delimited(
            ListSpec::angle("generic parameter", &[TokT::Identifier]),
            |p| p.expect_ident("`<`"),
        );
        Generics(Some(list.into_ast()))
    }

    /// `<ConcreteA, ConcreteB>` if the current token is `<`, else nothing.
    pub(super) fn type_args(&mut self) -> Vec<TypeAst<'src>> {
        if !self.check(TokT::Less) {
            return vec![];
        }
        self.type_arg_list().items
    }

    /// `<ConcreteA, ConcreteB>`, starting at the `<`.
    pub(super) fn type_arg_list(&mut self) -> ParsedList<TypeAst<'src>> {
        self.delimited(ListSpec::angle("type argument", TYPE_START), Self::type_ast)
    }

    pub(super) fn type_ast(&mut self) -> PResult<TypeAst<'src>> {
        let start = self.start_span();
        let kind = match self.current.kind {
            TokT::Func => return self.function_type(),
            TokT::LeftParen => {
                let types = self.delimited(ListSpec::paren("type", TYPE_START), Self::type_ast);
                self.validate_tuple(&types, ParenContext::TupleType);
                TypeAstKind::Tuple(types.items)
            }
            TokT::Identifier => TypeAstKind::Named(self.named_type()),
            _ => return Err(self.err_unexpected(Expected::Type, None)),
        };
        let ty = TypeAst {
            kind,
            span: self.span_from(start),
        };
        Ok(self.optional_suffix(ty, start))
    }

    /// `ty?` if a `?` follows, else `ty`.
    fn optional_suffix(&mut self, ty: TypeAst<'src>, start: Span) -> TypeAst<'src> {
        if self.try_consume(TokT::Question) {
            TypeAst {
                kind: TypeAstKind::Optional(Box::new(ty)),
                span: self.span_from(start),
            }
        } else {
            ty
        }
    }

    /// `Name<A, B>`, starting at the name.
    fn named_type(&mut self) -> NamedType<'src> {
        let name = self.advance_ident();
        let args = self.type_args();
        NamedType {
            name,
            args,
            span: self.span_from(name.span),
        }
    }

    /// `Name<A, B>` where only a named type is allowed (`impl` targets and interfaces).
    pub(super) fn expect_named_type(&mut self, after: &'static str) -> PResult<NamedType<'src>> {
        if self.check(TokT::Identifier) {
            Ok(self.named_type())
        } else {
            Err(self.err_name(after))
        }
    }

    /// `func(T, U): R`, or `func?(T): R` for an optional function.
    fn function_type(&mut self) -> PResult<TypeAst<'src>> {
        let start = self.advance_kind(TokT::Func).span;
        let optional = self.check(TokT::QuestionParen);
        let open = if optional {
            self.advance().span
        } else {
            self.expect(TokT::LeftParen, "`func`")?.span
        };
        let params = self.delimited_after(
            open,
            ListSpec::paren("parameter type", TYPE_START),
            Self::type_ast,
        );
        let ret = self.type_annotation()?.map(Box::new);
        let func = TypeAst {
            kind: TypeAstKind::Fn {
                params: params.items,
                ret,
            },
            span: self.span_from(start),
        };
        if optional {
            Ok(TypeAst {
                kind: TypeAstKind::Optional(Box::new(func)),
                span: self.span_from(start),
            })
        } else {
            Ok(func)
        }
    }
}
