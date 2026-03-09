use crate::scanner::Span;
use crate::typechecker::core::error::report::Diagnostic;
use crate::typechecker::core::error::{
    context_short_message, kind_note, mismatch_label_message, BindingError, CallError, CallParamError,
    CallParamKind, DuplicateDefinition, GenericError, InvalidOperandTypes, TypeCheckerError,
    TypeRequirement,
};

impl Diagnostic for TypeCheckerError {
    fn span(&self) -> Span {
        match self {
            TypeCheckerError::SelfOutsideOfImpl { span } => *span,
            TypeCheckerError::UndefinedType { span, .. } => *span,
            TypeCheckerError::UndefinedVariable { span, .. } => *span,
            TypeCheckerError::CalleeIsNotCallable { span, .. } => *span,
            TypeCheckerError::TypeMismatch { primary_span, .. } => *primary_span,
            TypeCheckerError::OperatorConstraint { span, .. } => *span,
            TypeCheckerError::InvalidReturnOutsideFunction { span, .. } => *span,
            TypeCheckerError::MissingReturnStatement { span, .. } => *span,
            TypeCheckerError::TypeHasNoFields { span, .. } => *span,
            TypeCheckerError::UndefinedField { span, .. } => *span,
            TypeCheckerError::NonGlobalDeclaration { span, .. } => *span,
            TypeCheckerError::UndefinedMethod(inner) => inner.span,
            TypeCheckerError::StaticMethodOnInstance { span, .. } => *span,
            TypeCheckerError::MissingInterfaceMethods { span, .. } => *span,
            TypeCheckerError::InterfaceMethodTypeMismatch { span, .. } => *span,
            TypeCheckerError::UncoveredPattern { span, .. } => *span,
            TypeCheckerError::InvalidTupleIndex { span, .. } => *span,
            TypeCheckerError::InvalidIsUsage { span, .. } => *span,
            TypeCheckerError::InvalidOperandTypes(inner) => inner.span,
            TypeCheckerError::PrimitiveTypeShadowing { span, .. } => *span,
            TypeCheckerError::Duplicate(d) => d.span(),
            TypeCheckerError::CallParam(c) => c.span(),
            TypeCheckerError::Call(c) => c.span(),
            TypeCheckerError::Generic(g) => g.span(),
            TypeCheckerError::Binding(b) => b.span(),
        }
    }

    fn code(&self) -> &'static str {
        match self {
            TypeCheckerError::TypeMismatch { .. } => "E001",
            TypeCheckerError::OperatorConstraint { .. } => "E023", // was E001 — bug fix
            TypeCheckerError::UndefinedVariable { .. } => "E002",
            TypeCheckerError::UndefinedType { .. } => "E003",
            TypeCheckerError::UndefinedField { .. } => "E004",
            TypeCheckerError::UndefinedMethod(_) => "E005",
            TypeCheckerError::MissingReturnStatement { .. } => "E006",
            TypeCheckerError::InvalidReturnOutsideFunction { .. } => "E007",
            TypeCheckerError::CalleeIsNotCallable { .. } => "E008",
            TypeCheckerError::TypeHasNoFields { .. } => "E014",
            TypeCheckerError::SelfOutsideOfImpl { .. } => "E015",
            TypeCheckerError::NonGlobalDeclaration { .. } => "E017",
            TypeCheckerError::StaticMethodOnInstance { .. } => "E018",
            TypeCheckerError::MissingInterfaceMethods { .. } => "E020",
            TypeCheckerError::UncoveredPattern { .. } => "E021",
            TypeCheckerError::InvalidTupleIndex { .. } => "E022",
            TypeCheckerError::InvalidIsUsage { .. } => "E024",
            TypeCheckerError::InterfaceMethodTypeMismatch { .. } => "E028",
            TypeCheckerError::InvalidOperandTypes { .. } => "E030",
            TypeCheckerError::PrimitiveTypeShadowing { .. } => "E035",
            TypeCheckerError::Duplicate(d) => d.code(),
            TypeCheckerError::CallParam(c) => c.code(),
            TypeCheckerError::Call(c) => c.code(),
            TypeCheckerError::Generic(g) => g.code(),
            TypeCheckerError::Binding(b) => b.code(),
        }
    }

    fn title(&self) -> &'static str {
        match self {
            TypeCheckerError::SelfOutsideOfImpl { .. } => "Invalid use of 'self'",
            TypeCheckerError::UndefinedVariable { .. } => "Undefined variable",
            TypeCheckerError::CalleeIsNotCallable { .. } => "Cannot call non-function",
            TypeCheckerError::TypeMismatch { context, .. } => context_short_message(context),
            TypeCheckerError::OperatorConstraint { .. } => "Operator type constraint not met",
            TypeCheckerError::InvalidReturnOutsideFunction { .. } => "Invalid return statement",
            TypeCheckerError::MissingReturnStatement { .. } => "Missing return statement",
            TypeCheckerError::UndefinedType { .. } => "Undefined type",
            TypeCheckerError::TypeHasNoFields { .. } => "Type has no fields",
            TypeCheckerError::UndefinedField { .. } => "Undefined field",
            TypeCheckerError::NonGlobalDeclaration { .. } => "Non-global declaration",
            TypeCheckerError::UndefinedMethod(_) => "Undefined method",
            TypeCheckerError::StaticMethodOnInstance { .. } => {
                "Cannot call static method on instance"
            }
            TypeCheckerError::MissingInterfaceMethods { .. } => "Interface not implemented",
            TypeCheckerError::InterfaceMethodTypeMismatch { .. } => {
                "Interface method type mismatch"
            }
            TypeCheckerError::UncoveredPattern { .. } => "Uncovered pattern",
            TypeCheckerError::InvalidTupleIndex { .. } => "Invalid tuple index",
            TypeCheckerError::InvalidIsUsage { .. } => "Invalid 'is' operator usage",
            TypeCheckerError::InvalidOperandTypes { .. } => "Invalid operand types",
            TypeCheckerError::PrimitiveTypeShadowing { .. } => "Cannot shadow built-in type",
            TypeCheckerError::Duplicate(d) => d.title(),
            TypeCheckerError::CallParam(c) => c.title(),
            TypeCheckerError::Call(c) => c.title(),
            TypeCheckerError::Generic(g) => g.title(),
            TypeCheckerError::Binding(b) => b.title(),
        }
    }

    fn message(&self) -> String {
        match self {
            TypeCheckerError::SelfOutsideOfImpl { .. } => {
                "Cannot use 'self' and 'Self' outside of an implementation block.".to_string()
            }
            TypeCheckerError::UndefinedVariable { name, .. } => {
                format!("Undefined variable '{}'.", name)
            }
            TypeCheckerError::CalleeIsNotCallable { found, .. } => {
                format!(
                    "Cannot call type different from function. Found type '{}' where a function was expected.",
                    found
                )
            }
            TypeCheckerError::TypeMismatch {
                mismatch, context, ..
            } => {
                let root = format!(
                    "expected '{}' but found '{}'",
                    mismatch.expected, mismatch.found
                );
                if let Some(detail) = &mismatch.precise {
                    format!(
                        "{}. {}. {}",
                        context_short_message(context),
                        root,
                        kind_note(&mismatch.kind, detail)
                    )
                } else {
                    format!(
                        "{}. {}",
                        context_short_message(context),
                        mismatch_label_message(mismatch)
                    )
                }
            }
            TypeCheckerError::OperatorConstraint {
                operator,
                found,
                requirement,
                ..
            } => {
                let req_str = match requirement {
                    TypeRequirement::Exact(ty) => ty.to_string(),
                    TypeRequirement::Structural(s) => s.to_string(),
                };
                format!(
                    "Operator '{}' cannot be applied to type '{}'. Expected {}.",
                    operator, found, req_str
                )
            }
            TypeCheckerError::InvalidReturnOutsideFunction { .. } => {
                "Return statement outside of a function body.".to_string()
            }
            TypeCheckerError::MissingReturnStatement { .. } => {
                "Missing return statement.".to_string()
            }
            TypeCheckerError::UndefinedType { name, message, .. } => {
                format!("Undefined type '{}'. {}", name, message)
            }
            TypeCheckerError::TypeHasNoFields { found, .. } => {
                format!("Type '{}' has no fields.", found)
            }
            TypeCheckerError::UndefinedField {
                struct_name,
                field_name,
                ..
            } => {
                format!("Struct '{}' has no field '{}'.", struct_name, field_name)
            }
            TypeCheckerError::NonGlobalDeclaration { name, kind, .. } => {
                format!("{kind} '{}' is defined outside of global scope.", name)
            }
            TypeCheckerError::UndefinedMethod(inner) => {
                format!("Undefined method '{}' on type {}.", inner.method_name, inner.found)
            }
            TypeCheckerError::StaticMethodOnInstance { method_name, .. } => {
                format!(
                    "Cannot call static method '{}' on an instance.",
                    method_name
                )
            }
            TypeCheckerError::MissingInterfaceMethods {
                interface,
                missing_methods,
                ..
            } => {
                format!(
                    "Type does not implement interface '{}': missing {}.",
                    interface,
                    missing_methods.join(", ")
                )
            }
            TypeCheckerError::InterfaceMethodTypeMismatch {
                method_name,
                interface,
                type_mismatch,
                ..
            } => {
                format!(
                    "Method '{}' does not satisfy interface '{}': expected '{}' but found '{}'.",
                    method_name, interface, type_mismatch.expected, type_mismatch.found
                )
            }
            TypeCheckerError::UncoveredPattern { variant, .. } => {
                format!("Uncovered pattern matching variant '{}'.", variant)
            }
            TypeCheckerError::InvalidTupleIndex {
                tuple_type, index, ..
            } => {
                format!("Invalid tuple index '{}' of {}.", index, tuple_type)
            }
            TypeCheckerError::InvalidIsUsage { message, .. } => {
                format!("Invalid usage of 'is' operator. {}", message)
            }
            TypeCheckerError::InvalidOperandTypes(inner) => {
                let InvalidOperandTypes {
                    operator,
                    left,
                    right,
                    ..
                } = &**inner;
                format!(
                    "Operator '{}' cannot be applied to types '{}' and '{}'.",
                    operator, left, right
                )
            }
            TypeCheckerError::PrimitiveTypeShadowing { name, .. } => {
                format!("Cannot redeclare built-in type '{}'.", name)
            }
            TypeCheckerError::Duplicate(d) => d.message(),
            TypeCheckerError::CallParam(c) => c.message(),
            TypeCheckerError::Call(c) => c.message(),
            TypeCheckerError::Generic(g) => g.message(),
            TypeCheckerError::Binding(b) => b.message(),
        }
    }
}

impl Diagnostic for BindingError {
    fn span(&self) -> Span {
        match self {
            BindingError::Redeclaration { span, .. } => *span,
            BindingError::Immutable { span, .. } => *span,
            BindingError::Captured { span, .. } => *span,
        }
    }
    fn code(&self) -> &'static str {
        match self {
            BindingError::Redeclaration { .. } => "E019",
            BindingError::Immutable { .. } => "E029",
            BindingError::Captured { .. } => "E016",
        }
    }
    fn title(&self) -> &'static str {
        match self {
            BindingError::Redeclaration { .. } => "Redeclaration",
            BindingError::Immutable { .. } => "Cannot assign to binding",
            BindingError::Captured { .. } => "Cannot assign to captured variable",
        }
    }
    fn message(&self) -> String {
        match self {
            BindingError::Redeclaration {
                name,
                original_kind,
                ..
            } => {
                format!("Redeclaration of {} '{}'.", original_kind.as_str(), name)
            }
            BindingError::Immutable { kind, name, .. } => {
                format!("Cannot assign to {} '{}'.", kind.as_str(), name)
            }
            BindingError::Captured { name, .. } => {
                format!("Cannot assign to captured variable '{}'.", name)
            }
        }
    }
}

impl Diagnostic for DuplicateDefinition {
    fn span(&self) -> Span {
        self.span
    }
    fn code(&self) -> &'static str {
        self.kind.code()
    }
    fn title(&self) -> &'static str {
        self.kind.short_title()
    }
    fn message(&self) -> String {
        self.kind.report_message(&self.name)
    }
}

impl Diagnostic for CallParamError {
    fn span(&self) -> Span {
        self.span
    }
    fn code(&self) -> &'static str {
        match self.kind {
            CallParamKind::Missing => "E010",
            CallParamKind::Undefined => "E012",
        }
    }
    fn title(&self) -> &'static str {
        match self.kind {
            CallParamKind::Missing => "Missing argument",
            CallParamKind::Undefined => "Undefined parameter",
        }
    }
    fn message(&self) -> String {
        match self.kind {
            CallParamKind::Missing => format!("Missing argument '{}'.", self.param_name),
            CallParamKind::Undefined => format!("Undefined parameter '{}'.", self.param_name),
        }
    }
}

impl Diagnostic for CallError {
    fn span(&self) -> Span {
        match self {
            CallError::TooMany { span, .. } => *span,
            CallError::DuplicateArgument { span, .. } => *span,
            CallError::PositionalAfterNamed { span, .. } => *span,
        }
    }
    fn code(&self) -> &'static str {
        match self {
            CallError::TooMany { .. } => "E009",
            CallError::DuplicateArgument { .. } => "E011",
            CallError::PositionalAfterNamed { .. } => "E013",
        }
    }
    fn title(&self) -> &'static str {
        match self {
            CallError::TooMany { .. } => "Too many arguments",
            CallError::DuplicateArgument { .. } => "Duplicate argument",
            CallError::PositionalAfterNamed { .. } => "Invalid argument order",
        }
    }
    fn message(&self) -> String {
        match self {
            CallError::TooMany {
                expected, found, ..
            } => {
                format!(
                    "Too many arguments. Expected {} but found {}.",
                    expected, found
                )
            }
            CallError::DuplicateArgument { name, .. } => {
                format!("Duplicate argument name '{}'.", name)
            }
            CallError::PositionalAfterNamed { message, .. } => {
                format!("Positional argument after named. {}", message)
            }
        }
    }
}

impl Diagnostic for GenericError {
    fn span(&self) -> Span {
        match self {
            GenericError::CannotInfer { span, .. } => *span,
            GenericError::CountMismatch { span, .. } => *span,
            GenericError::InvalidSpecification { span, .. } => *span,
        }
    }
    fn code(&self) -> &'static str {
        match self {
            GenericError::CannotInfer { .. } => "E025",
            GenericError::CountMismatch { .. } => "E027",
            GenericError::InvalidSpecification { .. } => "E026",
        }
    }
    fn title(&self) -> &'static str {
        match self {
            GenericError::CannotInfer { .. } => "Cannot infer type",
            GenericError::CountMismatch { .. } => "Generic count mismatch",
            GenericError::InvalidSpecification { .. } => "Invalid generic specification",
        }
    }
    fn message(&self) -> String {
        match self {
            GenericError::CannotInfer {
                uninferred_generics,
                ..
            } => {
                if uninferred_generics.is_empty() {
                    "Cannot infer type. Add an explicit type annotation.".to_string()
                } else {
                    format!(
                        "Cannot infer generic type parameter(s): {}. Add an explicit type annotation.",
                        uninferred_generics.join(", ")
                    )
                }
            }
            GenericError::CountMismatch {
                found,
                expected,
                type_name,
                ..
            } => {
                format!(
                    "Too many generics. Found {} expected {} for type '{}'.",
                    found, expected, type_name
                )
            }
            GenericError::InvalidSpecification { message, .. } => {
                format!("Invalid generic specification. {}", message)
            }
        }
    }
}
