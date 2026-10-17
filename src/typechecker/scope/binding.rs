//! Bindings: a name in a scope pointing at a target, plus where that name came from.
//!
//! Visibility of the *binding* (is the name exported?) lives here; visibility of the *definition*
//! (can its members be used elsewhere?) stays with the definition in `TypeSystem`.

use crate::scanner::Span;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Binding<T> {
    pub target: T,
    pub origin: Origin,
}

impl<T> Binding<T> {
    pub fn new(target: T, origin: Origin) -> Self {
        Binding { target, origin }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Origin {
    /// Primitives in the initial scope (present even with `--no-stdlib`). No span, never exported.
    Builtin,
    Defined {
        span: Span,
        public: bool,
    },
    Imported {
        import_span: Span,
    },
}

impl Origin {
    pub fn span(&self) -> Option<Span> {
        match self {
            Origin::Builtin => None,
            Origin::Defined { span, .. } | Origin::Imported { import_span: span } => Some(*span),
        }
    }

    /// Introduced by this module's own declaration.
    pub fn is_local(&self) -> bool {
        matches!(self, Origin::Defined { .. })
    }

    /// Part of this module's surface for importers.
    pub fn is_exported(&self) -> bool {
        matches!(self, Origin::Defined { public: true, .. })
    }
}
