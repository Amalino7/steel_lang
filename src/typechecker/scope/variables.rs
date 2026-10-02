use crate::compiler::analysis::ResolvedVar;
use crate::scanner::Span;
use crate::typechecker::Symbol;
use crate::typechecker::core::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct VariableContext {
    pub(crate) type_info: Type,
    pub(crate) name: Symbol,
    pub(crate) mutability: Mutability,
    pub(crate) kind: DeclarationKind,
    pub(crate) index: usize,
    pub(crate) span: Span,
    pub(crate) was_read: bool,
    pub(crate) was_written: bool,
    pub(crate) original_type: Option<(ResolvedVar, Type)>,
    /// Declared `public` in this module; imported copies are always private.
    pub(crate) is_public: bool,
}
/// Describes whether a variable can be reassigned.
/// Unique means it cannot be neither shadowed nor reassigned in the current scope.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Mutability {
    Mutable,
    Immutable,
    Unique,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeclarationKind {
    Variable,
    Function,
    Method,
    Parameter,
    /// A variable introduced by a destructuring pattern, e.g. `let (a, b) = …`.
    /// Immutable: reassignment is not allowed.
    Binding,
}

impl DeclarationKind {
    pub fn as_str(self) -> &'static str {
        match self {
            DeclarationKind::Variable => "variable",
            DeclarationKind::Function => "function",
            DeclarationKind::Method => "method",
            DeclarationKind::Parameter => "parameter",
            DeclarationKind::Binding => "destructured binding",
        }
    }
}

impl VariableContext {
    pub(crate) fn new(
        name: Symbol,
        type_info: Type,
        index: usize,
        span: Span,
        mutability: Mutability,
        kind: DeclarationKind,
    ) -> Self {
        VariableContext {
            name,
            type_info,
            index,
            span,
            mutability,
            kind,
            was_read: false,
            was_written: false,
            original_type: None,
            is_public: false,
        }
    }

    pub(crate) fn from_declaration(index: usize, decl: Declaration) -> Self {
        VariableContext {
            is_public: decl.is_public,
            ..VariableContext::new(
                decl.name,
                decl.type_info,
                index,
                decl.span,
                decl.mutability,
                decl.kind,
            )
        }
    }

    pub fn is_reassignable(&self) -> bool {
        self.mutability == Mutability::Mutable
    }
}

pub struct Declaration {
    pub name: Symbol,
    pub type_info: Type,
    pub span: Span,
    pub mutability: Mutability,
    pub kind: DeclarationKind,
    pub is_public: bool,
}

impl Declaration {
    pub fn public(mut self, is_public: bool) -> Self {
        self.is_public = is_public;
        self
    }

    // TODO use when adding mutability to variables.
    #[allow(dead_code)]
    pub fn variable(name: Symbol, type_info: Type, span: Span) -> Self {
        Declaration {
            name,
            type_info,
            span,
            mutability: Mutability::Immutable,
            kind: DeclarationKind::Variable,
            is_public: false,
        }
    }

    pub fn mutable(name: Symbol, type_info: Type, span: Span) -> Self {
        Declaration {
            name,
            type_info,
            span,
            mutability: Mutability::Mutable,
            kind: DeclarationKind::Variable,
            is_public: false,
        }
    }

    /// Creates an immutable binding introduced by a destructuring pattern.
    pub fn binding(name: Symbol, type_info: Type, span: Span) -> Self {
        Declaration {
            name,
            type_info,
            span,
            mutability: Mutability::Immutable,
            kind: DeclarationKind::Binding,
            is_public: false,
        }
    }

    pub fn parameter(name: Symbol, type_info: Type, span: Span) -> Self {
        Declaration {
            name,
            type_info,
            span,
            mutability: Mutability::Unique,
            kind: DeclarationKind::Parameter,
            is_public: false,
        }
    }

    pub fn global_function(name: Symbol, type_info: Type, span: Span) -> Self {
        Declaration {
            name,
            type_info,
            span,
            mutability: Mutability::Unique,
            kind: DeclarationKind::Function,
            is_public: false,
        }
    }
    pub fn function(name: Symbol, type_info: Type, span: Span) -> Self {
        Declaration {
            name,
            type_info,
            span,
            mutability: Mutability::Immutable,
            kind: DeclarationKind::Function,
            is_public: false,
        }
    }
}
