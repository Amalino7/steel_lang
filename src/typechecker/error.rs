use crate::scanner::Span;
use crate::typechecker::types::Type;
use ariadne::{Color, Label, Report, ReportKind};
use std::ops::Range;

#[derive(Debug, Clone)]
pub enum TypeCheckerError {
    UndefinedType {
        name: String,
        span: Span,
        message: &'static str,
    },
    UndefinedVariable {
        name: String,
        span: Span,
    },
    CalleeIsNotCallable {
        found: Type,
        span: Span,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        span: Span,
        message: &'static str,
    },
    ComplexTypeMismatch {
        expected: Type,
        found: Type,
        message: String,
        span: Span,
    },
    InvalidReturnOutsideFunction {
        span: Span,
    },
    UnreachableCode {
        span: Span,
    },
    MissingReturnStatement {
        fn_span: Span, // Signature
        fn_name: String,
        span: Span, // Point to the closing brace of the function
    },
    AssignmentToCapturedVariable {
        name: String,
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
    },
    StructOutsideOfGlobalScope {
        name: String,
        span: Span,
    },
    UndefinedMethod {
        span: Span,
        found: Type,
        method_name: String,
    },
    StaticMethodOnInstance {
        method_name: String,
        span: Span,
    },
    Redeclaration {
        name: String,
        span: Span,
    },
    DoesNotImplementInterface {
        missing_methods: Vec<String>,
        interface: String,
        span: Span,
    },
    UncoveredPattern {
        variant: String,
        span: Span,
    },
    TooManyArguments {
        expected: usize,
        found: usize,
        span: Span,
    },
    DuplicateArgument {
        name: String,
        span: Span,
    },
    UndefinedParameter {
        param_name: String,
        span: Span,
    },
    MissingArgument {
        param_name: String,
        span: Span,
    },
    PositionalArgumentAfterNamed {
        message: &'static str,
        span: Span,
    },
    InvalidTupleIndex {
        tuple_type: Type,
        index: String,
        span: Span,
    },
    UnreachablePattern {
        span: Span,
        message: String,
    },
    InvalidIsUsage {
        span: Span,
        message: &'static str,
    },
    MissingGeneric {
        ty_name: String,
        generic_name: String,
        span: Span,
    },
    CannotInferType {
        span: Span,
        uninferred_generics: Vec<String>,
    },
    InvalidGenericSpecification {
        span: Span,
        message: String,
    },
    TooManyGenerics {
        span: Span,
        found: usize,
        expected: usize,
        type_name: String,
    },
}
impl std::fmt::Display for TypeCheckerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

pub trait Recoverable<T> {
    /// Pushes error to a collection and returns a fallback value.
    fn recover(self, errors: &mut Vec<TypeCheckerError>, fallback: T) -> T;
    fn ok_log(self, errors: &mut Vec<TypeCheckerError>) -> Option<T>;
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
    fn ok_log(self, errors: &mut Vec<TypeCheckerError>) -> Option<T> {
        match self {
            Ok(val) => Some(val),
            Err(err) => {
                errors.push(err);
                None
            }
        }
    }
}

impl TypeCheckerError {
    pub fn create_report<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        let offset = self.span().start;

        let mut report = Report::build(ReportKind::Error, source_id, offset).with_code("TypeError");

        match self {
            TypeCheckerError::TypeMismatch {
                expected,
                found,
                span,
                message,
            } => {
                let mut labels = vec![];

                labels.push(
                    Label::new((source_id, span.to_range()))
                        .with_message(format!(
                            "Found type '{}' here but expected {}.",
                            found, expected
                        ))
                        .with_color(Color::Red),
                );

                // // 2. Secondary Label (Yellow): Why we expected something else
                // if let Some(origin) = expected_span {
                //     labels.push(
                //         Label::new(("src.lang", origin.to_range()))
                //             .with_message(format!("Expected type '{}' because of this", expected))
                //             .with_color(Color::Yellow),
                //     );
                // }

                report = report.with_message(message).with_labels(labels);
            }

            TypeCheckerError::MissingReturnStatement {
                span,
                fn_span,
                fn_name,
            } => {
                report = report
                    .with_message(format!(
                        "Function '{}' is missing a return statement",
                        fn_name
                    ))
                    .with_labels(vec![
                        Label::new((source_id, span.to_range()))
                            .with_message("Implicitly returns () here")
                            .with_color(Color::Red),
                        Label::new((source_id, fn_span.to_range()))
                            .with_message(format!("'{}' declared here", fn_name))
                            .with_color(Color::Blue),
                    ])
                    .with_note("Functions with a return type must return a value on all paths.");
            }
            err => {
                report = report.with_message(err.message()).with_label(
                    Label::new((source_id, err.span().to_range()))
                        .with_message(err.message())
                        .with_color(Color::Red),
                );
            }
        }

        report.finish()
    }

    // This is the method main.rs will call to tell Ariadne WHERE to point
    pub fn span(&self) -> Span {
        match self {
            TypeCheckerError::UndefinedType { span, .. } => *span,
            TypeCheckerError::UndefinedVariable { span, .. } => *span,
            TypeCheckerError::CalleeIsNotCallable { span, .. } => *span,
            TypeCheckerError::TypeMismatch { span, .. } => *span,
            TypeCheckerError::ComplexTypeMismatch { span, .. } => *span,
            TypeCheckerError::InvalidReturnOutsideFunction { span, .. } => *span,
            TypeCheckerError::UnreachableCode { span, .. } => *span,
            TypeCheckerError::MissingReturnStatement { span, .. } => *span,
            TypeCheckerError::AssignmentToCapturedVariable { span, .. } => *span,
            TypeCheckerError::TypeHasNoFields { span, .. } => *span,
            TypeCheckerError::UndefinedField { span, .. } => *span,
            TypeCheckerError::StructOutsideOfGlobalScope { span, .. } => *span,
            TypeCheckerError::UndefinedMethod { span, .. } => *span,
            TypeCheckerError::StaticMethodOnInstance { span, .. } => *span,
            TypeCheckerError::Redeclaration { span, .. } => *span,
            TypeCheckerError::DoesNotImplementInterface { span, .. } => *span,
            TypeCheckerError::UncoveredPattern { span, .. } => *span,
            TypeCheckerError::TooManyArguments { span, .. } => *span,
            TypeCheckerError::DuplicateArgument { span, .. } => *span,
            TypeCheckerError::UndefinedParameter { span, .. } => *span,
            TypeCheckerError::MissingArgument { span, .. } => *span,
            TypeCheckerError::PositionalArgumentAfterNamed { span, .. } => *span,
            TypeCheckerError::InvalidTupleIndex { span, .. } => *span,
            TypeCheckerError::UnreachablePattern { span, .. } => *span,
            TypeCheckerError::InvalidIsUsage { span, .. } => *span,
            TypeCheckerError::MissingGeneric { span, .. } => *span,
            TypeCheckerError::CannotInferType { span, .. } => *span,
            TypeCheckerError::InvalidGenericSpecification { span, .. } => *span,
            TypeCheckerError::TooManyGenerics { span, .. } => *span,
        }
    }

    // This is the method main.rs will call to tell Ariadne WHAT to say
    pub fn message(&self) -> String {
        match self {
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
                expected,
                found,
                message,
                ..
            } => {
                format!(
                    "{}. Expected '{}' but found '{}'.",
                    message, expected, found
                )
            }
            TypeCheckerError::ComplexTypeMismatch {
                expected,
                found,
                message,
                ..
            } => {
                format!(
                    "Type mismatch. Expected '{}' but found '{}'.\nPrecise: {}",
                    expected, found, message
                )
            }
            TypeCheckerError::InvalidReturnOutsideFunction { .. } => {
                "Return statement outside of a function body.".to_string()
            }
            TypeCheckerError::UnreachableCode { .. } => "Unreachable code detected.".to_string(),
            TypeCheckerError::MissingReturnStatement { .. } => {
                "Missing return statement.".to_string()
            }
            TypeCheckerError::AssignmentToCapturedVariable { name, .. } => {
                format!("Cannot assign to captured variable '{}'.", name)
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
            TypeCheckerError::StructOutsideOfGlobalScope { name, .. } => {
                format!("Struct '{}' is defined outside of global scope.", name)
            }
            TypeCheckerError::UndefinedMethod {
                method_name, found, ..
            } => {
                format!("Undefined method '{}' on type {}.", method_name, found)
            }
            TypeCheckerError::StaticMethodOnInstance { method_name, .. } => {
                format!(
                    "Cannot call static method '{}' on an instance.",
                    method_name
                )
            }
            TypeCheckerError::Redeclaration { name, .. } => {
                format!("Redeclaration of type or variable '{}'.", name)
            }
            TypeCheckerError::DoesNotImplementInterface {
                missing_methods,
                interface,
                ..
            } => {
                format!(
                    "Type does not implement interface '{}'. Missing methods or mismatched types: {}.",
                    interface,
                    missing_methods.join(", ")
                )
            }
            TypeCheckerError::UncoveredPattern { variant, .. } => {
                format!("Uncovered pattern matching variant '{}'.", variant)
            }
            TypeCheckerError::TooManyArguments {
                expected, found, ..
            } => {
                format!(
                    "Too many arguments. Expected {} but found {}.",
                    expected, found
                )
            }
            TypeCheckerError::DuplicateArgument { name, .. } => {
                format!("Duplicate argument name '{}'.", name)
            }
            TypeCheckerError::UndefinedParameter { param_name, .. } => {
                format!("Undefined parameter '{}'.", param_name)
            }
            TypeCheckerError::MissingArgument { param_name, .. } => {
                format!("Missing argument '{}'.", param_name,)
            }
            TypeCheckerError::PositionalArgumentAfterNamed { message, .. } => {
                format!("Positional argument after named. {}", message)
            }
            TypeCheckerError::InvalidTupleIndex {
                tuple_type, index, ..
            } => {
                format!("Invalid tuple index '{}' of {}.", index, tuple_type)
            }
            TypeCheckerError::UnreachablePattern { message, .. } => {
                format!("Unreachable Pattern. {}", message)
            }
            TypeCheckerError::InvalidIsUsage { message, .. } => {
                format!("Invalid usage of 'is' operator. {}", message)
            }
            TypeCheckerError::MissingGeneric {
                ty_name,
                generic_name,
                ..
            } => {
                format!(
                    "Missing generic on type '{}'. Name: {}",
                    ty_name, generic_name
                )
            }
            TypeCheckerError::CannotInferType {
                uninferred_generics,
                ..
            } => {
                format!(
                    "Cannot infer generic type for: {}. Use explicit annotations.",
                    uninferred_generics.join(", ")
                )
            }
            TypeCheckerError::InvalidGenericSpecification { message, .. } => {
                format!("Invalid generic specification. {}", message)
            }
            TypeCheckerError::TooManyGenerics {
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
        }
    }
}
