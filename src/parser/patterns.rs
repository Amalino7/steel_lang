//! Patterns and bindings

use crate::parser::{
    PResult, Parser, TokT,
    ast::{Binding, BindingKind, FieldBinding, Ident, Pattern, PatternKind},
    error::{ParenContext, ParseError, ParseErrorKind},
    lists::{ListSpec, ParsedList},
};

impl<'src> Parser<'src> {
    pub(super) fn pattern(&mut self) -> PResult<Pattern<'src>> {
        let start = self.start_span();
        if self.check(TokT::Identifier) && !self.check_next(TokT::Dot) {
            let name = self.advance_ident();
            let kind = if name.name == WILDCARD {
                PatternKind::Wildcard
            } else {
                PatternKind::Name(name)
            };
            return Ok(Pattern {
                kind,
                span: name.span,
            });
        }

        let enum_name = self.try_ident();
        self.expect(TokT::Dot, "enum name")?;
        let variant = self.expect_ident("`.`")?;

        let payload = self.variant_payload(variant);

        Ok(Pattern {
            kind: PatternKind::Variant {
                enum_name,
                variant,
                payload,
            },
            span: self.span_from(start),
        })
    }

    /// `after` names what precedes the binding, for the "expected a name" error.
    pub(super) fn binding(&mut self, after: &'static str) -> PResult<Binding<'src>> {
        let start = self.start_span();
        let kind = match self.current.kind {
            TokT::LeftParen => self.tuple_binding(),
            TokT::Identifier => {
                let name = self.advance_ident();
                if self.check(TokT::LeftParen) {
                    let fields = self.delimited(STRUCT_DESTRUCTURE, Self::field_binding);
                    BindingKind::Struct {
                        name,
                        fields: fields.items,
                    }
                } else {
                    name_binding(name)
                }
            }
            // Always an error here; reports keywords used as names specifically.
            _ => return Err(self.err_name(after)),
        };
        Ok(Binding {
            kind,
            span: self.span_from(start),
        })
    }

    fn variant_payload(&mut self, variant: Ident<'src>) -> Option<Binding<'src>> {
        if !self.check(TokT::LeftParen) {
            return None;
        }
        // `(` is consumed first: telling `.V(x: a)` from `.V(a)` needs the two tokens after it.
        let open = self.advance().span;

        if self.at_field_binding() {
            let fields = self.delimited_after(open, STRUCT_DESTRUCTURE, Self::field_binding);
            return Some(Binding {
                span: variant.span.merge(fields.close),
                kind: BindingKind::Struct {
                    name: variant,
                    fields: fields.items,
                },
            });
        }

        let mut bindings = self.delimited_after(open, TUPLE_DESTRUCTURE, Self::tuple_element);

        match bindings.items.len() {
            0 => {
                if !bindings.failed {
                    self.report(
                        ParseErrorKind::EmptyParens(ParenContext::PatternPayload),
                        bindings.span(),
                    );
                }
                None
            }
            1 => bindings.items.pop(),
            _ => Some(Binding {
                span: bindings.span(),
                kind: BindingKind::Tuple(bindings.items),
            }),
        }
    }

    /// `field: binding`, or `: name` as shorthand for `name: name`.
    pub(super) fn field_binding(&mut self) -> PResult<FieldBinding<'src>> {
        let start = self.start_span();
        let field = self.try_ident();
        self.expect(TokT::Colon, "field name")?;
        let binding = self.binding("`:`")?;
        let field = match (field, &binding.kind) {
            (Some(field), _) => field,
            (None, BindingKind::Name(name)) => *name,
            (None, _) => {
                return Err(ParseError::new(
                    ParseErrorKind::FieldPatternWithoutName,
                    binding.span,
                ));
            }
        };
        Ok(FieldBinding {
            field,
            binding,
            span: self.span_from(start),
        })
    }

    fn tuple_binding(&mut self) -> BindingKind<'src> {
        let elems = self.delimited(TUPLE_DESTRUCTURE, Self::tuple_element);
        self.validate_tuple(&elems, ParenContext::TuplePattern);
        BindingKind::Tuple(elems.items)
    }

    fn tuple_element(&mut self) -> PResult<Binding<'src>> {
        self.binding("`(` or `,`")
    }

    /// Parenthesised lists that are tuples: `()` is an error, and one item needs a trailing comma (`(x,)`), since
    /// `(x)` would be a grouping.
    pub(super) fn validate_tuple<T>(&mut self, list: &ParsedList<T>, context: ParenContext) {
        if list.failed {
            return;
        }
        let kind = match (list.items.len(), list.trailing) {
            (0, _) => ParseErrorKind::EmptyParens(context),
            (1, None) => ParseErrorKind::OneElementTuple(context),
            _ => return,
        };
        self.report(kind, list.span());
    }

    /// Checks if it as field binding  `:a` or `a:`
    fn at_field_binding(&self) -> bool {
        self.check(TokT::Colon) || (self.check(TokT::Identifier) && self.check_next(TokT::Colon))
    }
}

/// FIRST set of a binding: the tokens `binding` accepts.
pub(super) const BINDING_START: &[TokT] = &[TokT::Identifier, TokT::LeftParen];

/// `(key: binding, :name)` after a struct or variant name. Shared by `let` destructuring and match patterns.
pub(super) const STRUCT_DESTRUCTURE: ListSpec =
    ListSpec::paren("field binding", &[TokT::Identifier, TokT::Colon]);

pub(super) const TUPLE_DESTRUCTURE: ListSpec = ListSpec::paren("binding", BINDING_START);

/// The name that matches anything and binds nothing.
pub(super) const WILDCARD: &str = "_";

/// `x`, or the wildcard for `_`.
fn name_binding(name: Ident<'_>) -> BindingKind<'_> {
    if name.name == WILDCARD {
        BindingKind::Wildcard
    } else {
        BindingKind::Name(name)
    }
}
