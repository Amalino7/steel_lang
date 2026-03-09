mod diagnostic;
mod render;
mod report;
mod warning;

pub use warning::*;

use self::report::Diagnostic;
use crate::scanner::Span;
use crate::typechecker::core::error::report::ReportBuilder;
use crate::typechecker::core::types::Type;
use crate::typechecker::inference::{InferenceContext, UnificationError, UnificationErrorKind};
use crate::typechecker::scope::variables::DeclarationKind;
use ariadne::Report;
use std::ops::Range;

/// The leaf-level types at the exact point where unification failed.
/// Only present when they differ from the root types in [`Mismatch`].
#[derive(Debug, Clone)]
pub struct MismatchDetail {
    pub expected: Type,
    pub found: Type,
}

/// The precise failure from unification.
///
/// `expected`/`found` are the **root**  types being compared.
/// When the failure is deep inside the type structure, `precise` holds the
/// leaf types at the exact point of divergence.
#[derive(Debug, Clone)]
pub struct Mismatch {
    pub expected: Type,
    pub found: Type,
    pub kind: UnificationErrorKind,
    pub precise: Option<MismatchDetail>,
}

impl Mismatch {
    pub fn simple(expected: Type, found: Type) -> Self {
        Self {
            expected,
            found,
            kind: UnificationErrorKind::TypeMismatch,
            precise: None,
        }
    }

    pub fn enriched(
        root_expected: &Type,
        root_found: &Type,
        unif_err: UnificationError,
        infer: &InferenceContext,
    ) -> Self {
        let root_expected = infer.substitute(root_expected);
        let root_found = infer.substitute(root_found);
        let leaf_expected = infer.substitute(&unif_err.expected);
        let leaf_found = infer.substitute(&unif_err.found);

        let precise = if leaf_expected != root_expected || leaf_found != root_found {
            Some(MismatchDetail {
                expected: leaf_expected,
                found: leaf_found,
            })
        } else {
            None
        };

        Mismatch {
            expected: root_expected,
            found: root_found,
            kind: unif_err.kind,
            precise,
        }
    }
}

impl From<UnificationError> for Mismatch {
    fn from(e: UnificationError) -> Self {
        Self {
            expected: e.expected,
            found: e.found,
            kind: e.kind,
            precise: None,
        }
    }
}

/// Drives the wording of the error message.
#[derive(Debug, Clone)]
pub enum MismatchContext {
    Return,
    Let,
    Field {
        name: String,
    },
    Argument {
        param_name: String,
    },
    ListElement,
    Condition,
    Coalesce,
    TupleElement {
        index: usize,
    },
    IndexValue,
    /// Placeholder
    Generic,
}

/// The type requirement placed on an operand by an operator.
#[derive(Debug, Clone)]
pub enum TypeRequirement {
    Exact(Type),
    Structural(&'static str),
}

#[derive(Debug, Clone)]
pub enum Operand {
    Lhs,
    Rhs,
    Unary,
}

/// What kind of item was declared twice.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DuplicateKind {
    Field,
    Variant,
    Type,
    GenericParam,
}

impl DuplicateKind {
    fn noun(self) -> &'static str {
        match self {
            Self::Field => "field",
            Self::Variant => "variant",
            Self::Type => "type",
            Self::GenericParam => "generic parameter",
        }
    }

    fn code(self) -> &'static str {
        match self {
            Self::Field => "E031",
            Self::Variant => "E032",
            Self::Type => "E033",
            Self::GenericParam => "E034",
        }
    }

    fn short_title(self) -> &'static str {
        match self {
            Self::Field => "Duplicate field",
            Self::Variant => "Duplicate variant",
            Self::Type => "Duplicate type declaration",
            Self::GenericParam => "Duplicate generic parameter",
        }
    }

    fn report_message(self, name: &str) -> String {
        match self {
            Self::Type => format!("Type '{}' is already defined", name),
            _ => format!("Duplicate {} '{}'", self.noun(), name),
        }
    }

    fn primary_label(self, name: &str) -> String {
        match self {
            Self::Type => format!("'{}' redefined here", name),
            Self::GenericParam => format!("'{}' used again here", name),
            _ => format!("'{}' declared again here", name),
        }
    }

    fn origin_label(self, name: &str) -> String {
        match self {
            Self::Type => format!("'{}' first defined here", name),
            _ => format!("'{}' first declared here", name),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DuplicateDefinition {
    pub kind: DuplicateKind,
    pub name: String,
    pub span: Span,
    pub original: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallParamKind {
    Missing,
    Undefined,
}

#[derive(Debug, Clone)]
pub struct CallParamError {
    pub kind: CallParamKind,
    pub param_name: String,
    pub span: Span,
    pub callee_origin: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum CallError {
    TooMany {
        expected: usize,
        found: usize,
        span: Span,
        callee: Span,
        callee_origin: Option<Span>,
    },
    DuplicateArgument {
        name: String,
        span: Span,
    },
    PositionalAfterNamed {
        message: &'static str,
        span: Span,
    },
}

impl CallError {
    pub(crate) fn render<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        match self {
            CallError::TooMany {
                expected,
                found,
                span,
                callee,
                callee_origin,
            } => ReportBuilder::error(source_id, *span, self.code(), "Too many arguments")
                .primary(
                    *span,
                    format!(
                        "Too many arguments. Expected {} but found {}.",
                        expected, found
                    ),
                )
                .secondary(*callee, "Called function is here")
                .optional_origin(*callee_origin, "Callee declared here")
                .finish(),
            CallError::DuplicateArgument { name, span } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Duplicate argument name '{}'", name),
            )
            .primary(
                *span,
                format!("'{}' is already passed as an argument", name),
            )
            .finish(),
            CallError::PositionalAfterNamed { message, span } => {
                ReportBuilder::error(source_id, *span, self.code(), "Invalid argument order")
                    .primary(*span, *message)
                    .finish()
            }
        }
    }
}

/// Errors arising during generic resolution.
/// Replaces `CannotInferType`, `GenericCountMismatch`, `InvalidGenericSpecification`.
#[derive(Debug, Clone)]
pub enum GenericError {
    CannotInfer {
        span: Span,
        uninferred_generics: Vec<String>,
    },
    CountMismatch {
        span: Span,
        found: usize,
        expected: usize,
        type_name: String,
    },
    InvalidSpecification {
        span: Span,
        message: String,
    },
}

/// Errors about variable/binding mutation and redeclaration rules.
#[derive(Debug, Clone)]
pub enum BindingError {
    Redeclaration {
        name: String,
        span: Span,
        original: Span,
        original_kind: DeclarationKind,
    },
    Immutable {
        kind: DeclarationKind,
        name: String,
        span: Span,
        definition_span: Span,
    },
    Captured {
        name: String,
        span: Span,
        capture_origin: Span,
    },
}

#[derive(Debug, Clone)]
pub enum TypeCheckerError {
    SelfOutsideOfImpl {
        span: Span,
    },
    UndefinedType {
        name: String,
        span: Span,
        message: &'static str,
    },
    UndefinedVariable {
        name: String,
        span: Span,
        suggestions: Vec<String>,
    },
    CalleeIsNotCallable {
        found: Type,
        span: Span,
    },
    TypeMismatch {
        mismatch: Mismatch,
        context: MismatchContext,
        primary_span: Span,
        defined_at: Option<Span>,
    },
    OperatorConstraint {
        operator: &'static str,
        operand: Operand,
        found: Type,
        requirement: TypeRequirement,
        span: Span,
    },
    InvalidReturnOutsideFunction {
        span: Span,
    },
    MissingReturnStatement {
        fn_span: Span,
        fn_name: String,
        span: Span,
    },
    TypeHasNoFields {
        found: Type,
        span: Span,
    },
    UndefinedField {
        struct_name: String,
        field_name: String,
        span: Span,
        struct_origin: Option<Span>,
        suggestions: Vec<String>,
    },
    NonGlobalDeclaration {
        name: String,
        kind: &'static str,
        span: Span,
    },
    UndefinedMethod {
        span: Span,
        found: Type,
        method_name: String,
        type_origin: Option<Span>,
        suggestions: Vec<String>,
    },
    StaticMethodOnInstance {
        method_name: String,
        span: Span,
    },
    MissingInterfaceMethods {
        missing_methods: Vec<String>,
        interface: String,
        span: Span,
        interface_origin: Span,
    },
    InterfaceMethodTypeMismatch {
        method_name: String,
        interface: String,
        type_mismatch: Mismatch,
        span: Span,
        interface_origin: Span,
    },
    UncoveredPattern {
        variant: String,
        span: Span,
    },
    InvalidTupleIndex {
        tuple_type: Type,
        index: String,
        span: Span,
    },
    InvalidIsUsage {
        span: Span,
        message: &'static str,
    },
    InvalidOperandTypes {
        operator: String,
        left: Type,
        right: Type,
        span: Span,
        help: &'static str,
    },
    PrimitiveTypeShadowing {
        name: String,
        span: Span,
    },
    Duplicate(DuplicateDefinition),
    CallParam(CallParamError),
    Call(CallError),
    Generic(GenericError),
    Binding(BindingError),
}
impl TypeCheckerError {
    pub fn new_duplicate(kind: DuplicateKind, name: String, span: Span, original: Span) -> Self {
        Self::Duplicate(DuplicateDefinition {
            kind,
            name,
            span,
            original,
        })
    }
}
impl std::fmt::Display for TypeCheckerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

pub trait Recoverable<T> {
    /// Pushes error to a collection and returns a fallback value.
    fn recover(self, errors: &mut Vec<TypeCheckerError>, fallback: T) -> T;
    fn ok_or_report(self, errors: &mut Vec<TypeCheckerError>) -> Option<T>;
}

impl<T> Recoverable<T> for Result<T, TypeCheckerError> {
    fn recover(self, errors: &mut Vec<TypeCheckerError>, fallback: T) -> T {
        match self {
            Ok(val) => val,
            Err(err) => {
                errors.push(err);
                fallback
            }
        }
    }
    fn ok_or_report(self, errors: &mut Vec<TypeCheckerError>) -> Option<T> {
        match self {
            Ok(val) => Some(val),
            Err(err) => {
                errors.push(err);
                None
            }
        }
    }
}

fn mismatch_label_message(m: &Mismatch) -> String {
    match &m.kind {
        UnificationErrorKind::TypeMismatch | UnificationErrorKind::VarianceMismatch => {
            format!("found '{}', expected '{}'", m.found, m.expected)
        }
        UnificationErrorKind::ArityMismatch {
            expected_len,
            found_len,
        } => {
            format!(
                "expected {} type argument(s) but found {}",
                expected_len, found_len
            )
        }
        UnificationErrorKind::VarargNotAllowed => {
            "vararg functions cannot be passed as arguments".to_string()
        }
        UnificationErrorKind::GenericFunctionNotAllowed => {
            "generic functions must be specialized first — use .<Type> notation".to_string()
        }
        UnificationErrorKind::MetatypeNotUnifiable => {
            format!("type '{}' cannot be used as a value here", m.found)
        }
        UnificationErrorKind::OccursCheck => "recursive type definition detected".to_string(),
        UnificationErrorKind::InterfaceNotImplemented { interface } => {
            format!("'{}' does not implement interface '{}'", m.found, interface)
        }
    }
}

/// Render the note line for a deep mismatch, using leaf types + kind.
fn kind_note(kind: &UnificationErrorKind, detail: &MismatchDetail) -> String {
    match kind {
        UnificationErrorKind::TypeMismatch | UnificationErrorKind::VarianceMismatch => {
            format!(
                "'{}' is not compatible with '{}'",
                detail.found, detail.expected
            )
        }
        UnificationErrorKind::ArityMismatch {
            expected_len,
            found_len,
        } => {
            format!("expected {} elements but found {}", expected_len, found_len)
        }
        UnificationErrorKind::GenericFunctionNotAllowed => {
            "generic functions must be specialized first — use .<Type> notation".to_string()
        }
        UnificationErrorKind::InterfaceNotImplemented { interface } => {
            format!("'{}' does not implement '{}'", detail.found, interface)
        }
        UnificationErrorKind::OccursCheck => "recursive type detected".to_string(),
        UnificationErrorKind::VarargNotAllowed => {
            "vararg functions cannot be passed as arguments".to_string()
        }
        UnificationErrorKind::MetatypeNotUnifiable => {
            format!("'{}' cannot be used as a value", detail.found)
        }
    }
}

fn context_short_message(context: &MismatchContext) -> &'static str {
    match context {
        MismatchContext::Return => "Return type mismatch",
        MismatchContext::Let => "Type annotation mismatch",
        MismatchContext::Field { .. } => "Field type mismatch",
        MismatchContext::Argument { .. } => "Argument type mismatch",
        MismatchContext::ListElement => "List element type mismatch",
        MismatchContext::Condition => "Condition must be boolean",
        MismatchContext::Coalesce => "Coalesce type mismatch",
        MismatchContext::TupleElement { .. } => "Tuple element type mismatch",
        MismatchContext::IndexValue => "Index assignment type mismatch",
        MismatchContext::Generic => "Type mismatch",
    }
}

fn context_defined_at_message(context: &MismatchContext) -> String {
    match context {
        MismatchContext::Return => "return type defined here".to_string(),
        MismatchContext::Let => "type annotated here".to_string(),
        MismatchContext::Field { name } => format!("field '{}' defined here", name),
        MismatchContext::Argument { param_name } => {
            format!("parameter '{}' defined here", param_name)
        }
        MismatchContext::ListElement => "list element type inferred here".to_string(),
        MismatchContext::Condition => "condition required here".to_string(),
        MismatchContext::Coalesce => "coalesce type established here".to_string(),
        MismatchContext::TupleElement { index } => {
            format!("tuple element {} defined here", index)
        }
        MismatchContext::IndexValue => "index type required here".to_string(),
        MismatchContext::Generic => "type defined here".to_string(),
    }
}
