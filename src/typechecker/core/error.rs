use crate::scanner::Span;
use crate::typechecker::core::types::Type;
use crate::typechecker::scope::variables::DeclarationKind;
use ariadne::{Color, Label, Report, ReportKind};
use std::ops::Range;

#[derive(Debug, Clone)]
pub enum TypeCheckerWarning {
    UnusedBinding {
        name: String,
        span: Span,
    },
    SafeAccessOnNonOptional {
        span: Span,
    },
    RedundantForceUnwrap {
        span: Span,
    },
    ShadowedVariable {
        name: String,
        span: Span,
        original_span: Span,
    },
    UnreachableCode {
        span: Span,
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
        expected: Type,
        found: Type,
        span: Span,
        message: &'static str,
    },
    TypeMismatchWithOrigin {
        expected: Type,
        found: Type,
        span: Span,
        message: &'static str,
        expected_origin: Span,
    },
    ComplexTypeMismatch {
        expected: Type,
        found: Type,
        message: String,
        span: Span,
    },
    ComplexTypeMismatchWithOrigins {
        expected: Type,
        found: Type,
        message: String,
        span: Span,
        expected_origin: Option<Span>,
    },
    InvalidReturnOutsideFunction {
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
    AssignmentToImmutableBinding {
        kind: DeclarationKind,
        name: String,
        span: Span,
        definition_span: Span,
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
    StructOutsideOfGlobalScope {
        name: String,
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
    Redeclaration {
        name: String,
        span: Span,
        original: Span,
        original_kind: DeclarationKind,
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
        expected: Type,
        found: Type,
        span: Span, // points at the concrete method's name token
        interface_origin: Span,
    },
    UncoveredPattern {
        variant: String,
        span: Span,
    },
    TooManyArguments {
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
    UndefinedParameter {
        param_name: String,
        span: Span,
        callee_origin: Option<Span>,
    },
    MissingArgument {
        param_name: String,
        span: Span,
        callee_origin: Option<Span>,
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
    CannotInferType {
        span: Span,
        uninferred_generics: Vec<String>,
    },
    InvalidGenericSpecification {
        span: Span,
        message: String,
    },
    GenericCountMismatch {
        span: Span,
        found: usize,
        expected: usize,
        type_name: String,
    },
}

impl TypeCheckerError {
    pub fn with_origin(self, origin: Span) -> Self {
        match self {
            TypeCheckerError::TypeMismatch {
                expected,
                found,
                span,
                message,
            } => TypeCheckerError::TypeMismatchWithOrigin {
                expected,
                found,
                span,
                message,
                expected_origin: origin,
            },
            TypeCheckerError::ComplexTypeMismatch {
                expected,
                found,
                message,
                span,
            } => TypeCheckerError::ComplexTypeMismatchWithOrigins {
                expected,
                found,
                message,
                span,
                expected_origin: Some(origin),
            },
            other => other,
        }
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

        let mut report = Report::build(ReportKind::Error, source_id, offset).with_code(self.code());

        match self {
            TypeCheckerError::TypeMismatch {
                expected,
                found,
                span,
                message,
            } => {
                let labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!(
                            "Found type '{}' here but expected {}.",
                            found, expected
                        ))
                        .with_color(Color::Red),
                ];

                report = report.with_message(message).with_labels(labels);
            }

            TypeCheckerError::TypeMismatchWithOrigin {
                expected,
                found,
                span,
                message,
                expected_origin,
            } => {
                let mut labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!(
                            "Found type '{}' here but expected {}.",
                            found, expected
                        ))
                        .with_color(Color::Red),
                ];

                labels.push(
                    Label::new((source_id, expected_origin.to_range()))
                        .with_message(format!("Type '{}' originates here.", expected))
                        .with_color(Color::Yellow),
                );

                report = report.with_message(message).with_labels(labels);
            }

            TypeCheckerError::ComplexTypeMismatchWithOrigins {
                expected,
                found,
                message,
                span,
                expected_origin,
            } => {
                let mut labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!(
                            "Complex type mismatch: expected '{}' but found '{}'.",
                            expected, found
                        ))
                        .with_color(Color::Red),
                ];

                if let Some(exp_span) = expected_origin {
                    labels.push(
                        Label::new((source_id, exp_span.to_range()))
                            .with_message(format!("Expected type '{}' originated here.", expected))
                            .with_color(Color::Yellow),
                    );
                }

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
            TypeCheckerError::MissingInterfaceMethods {
                missing_methods,
                interface,
                span,
                interface_origin,
            } => {
                let labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!("Missing methods: {}", missing_methods.join(", ")))
                        .with_color(Color::Red),
                    Label::new((source_id, interface_origin.to_range()))
                        .with_message(format!("Interface '{}' declared here", interface))
                        .with_color(Color::Yellow),
                ];
                report = report
                    .with_message(format!(
                        "Methods needed to implement '{}' are missing",
                        interface
                    ))
                    .with_labels(labels);
            }

            TypeCheckerError::InterfaceMethodTypeMismatch {
                method_name,
                interface,
                expected,
                found,
                span,
                interface_origin,
            } => {
                let labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!("Expected '{}' but found '{}'", expected, found))
                        .with_color(Color::Red),
                    Label::new((source_id, interface_origin.to_range()))
                        .with_message(format!(
                            "'{}' is declared with type '{}' in interface '{}'",
                            method_name, expected, interface
                        ))
                        .with_color(Color::Yellow),
                ];
                report = report
                    .with_message(format!(
                        "Method '{}' does not satisfy interface '{}'",
                        method_name, interface
                    ))
                    .with_labels(labels);
            }
            TypeCheckerError::Redeclaration {
                name,
                span,
                original,
                original_kind,
            } => {
                let kind_str = original_kind.as_str();
                let labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!("Cannot redeclare {} '{}'", kind_str, name))
                        .with_color(Color::Red),
                    Label::new((source_id, original.to_range()))
                        .with_message(format!("{} '{}' originally declared here", kind_str, name))
                        .with_color(Color::Yellow),
                ];
                report = report
                    .with_message(format!("Redeclaration of {} '{}'", kind_str, name))
                    .with_labels(labels);
            }
            TypeCheckerError::AssignmentToImmutableBinding {
                kind,
                name,
                span,
                definition_span,
            } => {
                let kind_str = kind.as_str();
                let labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!("'{}' is a {}, not a variable", name, kind_str))
                        .with_color(Color::Red),
                    Label::new((source_id, definition_span.to_range()))
                        .with_message(format!("{} '{}' declared here", kind_str, name))
                        .with_color(Color::Blue),
                ];
                report = report
                    .with_message(format!("Cannot assign to {} '{}'", kind_str, name))
                    .with_labels(labels);
            }
            TypeCheckerError::UndefinedField {
                struct_name,
                field_name,
                span,
                struct_origin,
                suggestions,
            } => {
                let mut labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!(
                            "Field '{}' does not exist on struct '{}'",
                            field_name, struct_name
                        ))
                        .with_color(Color::Red),
                ];

                if let Some(origin) = struct_origin {
                    labels.push(
                        Label::new((source_id, origin.to_range()))
                            .with_message(format!("Struct '{}' defined here", struct_name))
                            .with_color(Color::Blue),
                    );
                }

                report = report
                    .with_message(format!("Undefined field '{}'", field_name))
                    .with_labels(labels);

                if !suggestions.is_empty() {
                    report =
                        report.with_help(format!("Did you mean '{}'?", suggestions.join("', '")));
                }
            }
            TypeCheckerError::UndefinedMethod {
                span,
                found,
                method_name,
                type_origin,
                suggestions,
            } => {
                let mut labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!(
                            "Method '{}' does not exist on type '{}'",
                            method_name, found
                        ))
                        .with_color(Color::Red),
                ];

                if let Some(origin) = type_origin {
                    labels.push(
                        Label::new((source_id, origin.to_range()))
                            .with_message(format!("Type '{}' defined here", found))
                            .with_color(Color::Blue),
                    );
                }

                report = report
                    .with_message(format!("Undefined method '{}'", method_name))
                    .with_labels(labels);

                if !suggestions.is_empty() {
                    report =
                        report.with_help(format!("Did you mean '{}'?", suggestions.join("', '")));
                }
            }
            TypeCheckerError::UndefinedVariable {
                name,
                span,
                suggestions,
            } => {
                report = report
                    .with_message(format!("Undefined variable '{}'", name))
                    .with_label(
                        Label::new((source_id, span.to_range()))
                            .with_message(format!(
                                "Variable '{}' is not defined in this scope",
                                name
                            ))
                            .with_color(Color::Red),
                    );

                if !suggestions.is_empty() {
                    report =
                        report.with_help(format!("Did you mean '{}'?", suggestions.join("', '")));
                }
            }
            TypeCheckerError::TooManyArguments {
                expected,
                found,
                span,
                callee,
                callee_origin,
            } => {
                let mut labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!(
                            "Too many arguments. Expected {} but found {}.",
                            expected, found
                        ))
                        .with_color(Color::Red),
                    Label::new((source_id, callee.to_range()))
                        .with_message("Called function is here")
                        .with_color(Color::Yellow),
                ];
                if let Some(origin) = callee_origin {
                    labels.push(
                        Label::new((source_id, origin.to_range()))
                            .with_message("Callee declared here")
                            .with_color(Color::Blue),
                    );
                }
                report = report
                    .with_message("Too many arguments")
                    .with_labels(labels);
            }
            TypeCheckerError::MissingArgument {
                param_name,
                span,
                callee_origin,
            } => {
                let mut labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!("Missing required argument '{}'", param_name))
                        .with_color(Color::Red),
                ];

                if let Some(origin) = callee_origin {
                    labels.push(
                        Label::new((source_id, origin.to_range()))
                            .with_message("Callee declared here")
                            .with_color(Color::Blue),
                    );
                }

                report = report
                    .with_message(format!("Missing argument '{}'", param_name))
                    .with_labels(labels);
            }
            TypeCheckerError::UndefinedParameter {
                param_name,
                span,
                callee_origin,
            } => {
                let mut labels = vec![
                    Label::new((source_id, span.to_range()))
                        .with_message(format!("Parameter '{}' does not exist", param_name))
                        .with_color(Color::Red),
                ];

                if let Some(origin) = callee_origin {
                    labels.push(
                        Label::new((source_id, origin.to_range()))
                            .with_message("Callee declared here")
                            .with_color(Color::Blue),
                    );
                }

                report = report
                    .with_message(format!("Undefined parameter '{}'", param_name))
                    .with_labels(labels);
            }
            TypeCheckerError::CannotInferType {
                span,
                uninferred_generics,
            } => {
                let label_msg = if uninferred_generics.is_empty() {
                    "cannot infer the type here".to_string()
                } else {
                    format!(
                        "cannot infer generic type parameter(s): {}",
                        uninferred_generics.join(", ")
                    )
                };
                report = report
                    .with_message("Cannot infer type")
                    .with_label(
                        Label::new((source_id, span.to_range()))
                            .with_message(label_msg)
                            .with_color(Color::Red),
                    )
                    .with_help(
                        "Add a type annotation or specify generics explicitly using .<Type> syntax",
                    );
            }

            err => {
                report = report.with_message(err.short_message()).with_label(
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
            TypeCheckerError::SelfOutsideOfImpl { span } => *span,
            TypeCheckerError::UndefinedType { span, .. } => *span,
            TypeCheckerError::UndefinedVariable { span, .. } => *span,
            TypeCheckerError::CalleeIsNotCallable { span, .. } => *span,
            TypeCheckerError::TypeMismatch { span, .. } => *span,
            TypeCheckerError::TypeMismatchWithOrigin { span, .. } => *span,
            TypeCheckerError::ComplexTypeMismatch { span, .. } => *span,
            TypeCheckerError::ComplexTypeMismatchWithOrigins { span, .. } => *span,
            TypeCheckerError::InvalidReturnOutsideFunction { span, .. } => *span,
            TypeCheckerError::MissingReturnStatement { span, .. } => *span,
            TypeCheckerError::AssignmentToCapturedVariable { span, .. } => *span,
            TypeCheckerError::TypeHasNoFields { span, .. } => *span,
            TypeCheckerError::UndefinedField { span, .. } => *span,
            TypeCheckerError::StructOutsideOfGlobalScope { span, .. } => *span,
            TypeCheckerError::UndefinedMethod { span, .. } => *span,
            TypeCheckerError::StaticMethodOnInstance { span, .. } => *span,
            TypeCheckerError::Redeclaration { span, .. } => *span,
            TypeCheckerError::AssignmentToImmutableBinding { span, .. } => *span,
            TypeCheckerError::MissingInterfaceMethods { span, .. } => *span,
            TypeCheckerError::InterfaceMethodTypeMismatch { span, .. } => *span,
            TypeCheckerError::UncoveredPattern { span, .. } => *span,
            TypeCheckerError::TooManyArguments { span, .. } => *span,
            TypeCheckerError::DuplicateArgument { span, .. } => *span,
            TypeCheckerError::UndefinedParameter { span, .. } => *span,
            TypeCheckerError::MissingArgument { span, .. } => *span,
            TypeCheckerError::PositionalArgumentAfterNamed { span, .. } => *span,
            TypeCheckerError::InvalidTupleIndex { span, .. } => *span,
            TypeCheckerError::UnreachablePattern { span, .. } => *span,
            TypeCheckerError::InvalidIsUsage { span, .. } => *span,
            TypeCheckerError::CannotInferType { span, .. } => *span,
            TypeCheckerError::InvalidGenericSpecification { span, .. } => *span,
            TypeCheckerError::GenericCountMismatch { span, .. } => *span,
        }
    }

    // This is the method main.rs will call to tell Ariadne WHAT to say
    pub fn message(&self) -> String {
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
            TypeCheckerError::TypeMismatchWithOrigin {
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
            TypeCheckerError::ComplexTypeMismatchWithOrigins {
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
            TypeCheckerError::Redeclaration {
                name,
                original_kind,
                ..
            } => {
                format!(
                    "Redeclaration of {} '{}'.",
                    original_kind.as_str(),
                    name
                )
            }
            TypeCheckerError::AssignmentToImmutableBinding { kind, name, .. } => {
                format!("Cannot assign to {} '{}'.", kind.as_str(), name)
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
                expected,
                found,
                ..
            } => {
                format!(
                    "Method '{}' does not satisfy interface '{}': expected '{}' but found '{}'.",
                    method_name, interface, expected, found
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
            TypeCheckerError::CannotInferType {
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
            TypeCheckerError::InvalidGenericSpecification { message, .. } => {
                format!("Invalid generic specification. {}", message)
            }
            TypeCheckerError::GenericCountMismatch {
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

    /// Returns an error code for this error type
    pub fn code(&self) -> &'static str {
        match self {
            TypeCheckerError::TypeMismatch { .. } => "E001",
            TypeCheckerError::TypeMismatchWithOrigin { .. } => "E001",
            TypeCheckerError::ComplexTypeMismatch { .. } => "E001",
            TypeCheckerError::ComplexTypeMismatchWithOrigins { .. } => "E001",
            TypeCheckerError::UndefinedVariable { .. } => "E002",
            TypeCheckerError::UndefinedType { .. } => "E003",
            TypeCheckerError::UndefinedField { .. } => "E004",
            TypeCheckerError::UndefinedMethod { .. } => "E005",
            TypeCheckerError::MissingReturnStatement { .. } => "E006",
            TypeCheckerError::InvalidReturnOutsideFunction { .. } => "E007",
            TypeCheckerError::CalleeIsNotCallable { .. } => "E008",
            TypeCheckerError::TooManyArguments { .. } => "E009",
            TypeCheckerError::MissingArgument { .. } => "E010",
            TypeCheckerError::DuplicateArgument { .. } => "E011",
            TypeCheckerError::UndefinedParameter { .. } => "E012",
            TypeCheckerError::PositionalArgumentAfterNamed { .. } => "E013",
            TypeCheckerError::TypeHasNoFields { .. } => "E014",
            TypeCheckerError::SelfOutsideOfImpl { .. } => "E015",
            TypeCheckerError::AssignmentToCapturedVariable { .. } => "E016",
            TypeCheckerError::StructOutsideOfGlobalScope { .. } => "E017",
            TypeCheckerError::StaticMethodOnInstance { .. } => "E018",
            TypeCheckerError::Redeclaration { .. } => "E019",
            TypeCheckerError::AssignmentToImmutableBinding { .. } => "E029",
            TypeCheckerError::MissingInterfaceMethods { .. } => "E020",
            TypeCheckerError::InterfaceMethodTypeMismatch { .. } => "E028",
            TypeCheckerError::UncoveredPattern { .. } => "E021",
            TypeCheckerError::InvalidTupleIndex { .. } => "E022",
            TypeCheckerError::UnreachablePattern { .. } => "E023",
            TypeCheckerError::InvalidIsUsage { .. } => "E024",
            TypeCheckerError::CannotInferType { .. } => "E025",
            TypeCheckerError::InvalidGenericSpecification { .. } => "E026",
            TypeCheckerError::GenericCountMismatch { .. } => "E027",
        }
    }

    /// Returns a short, category-style message for the error header
    pub fn short_message(&self) -> &'static str {
        match self {
            TypeCheckerError::SelfOutsideOfImpl { .. } => "Invalid use of 'self'",
            TypeCheckerError::UndefinedVariable { .. } => "Undefined variable",
            TypeCheckerError::CalleeIsNotCallable { .. } => "Cannot call non-function",
            TypeCheckerError::TypeMismatch { .. } => "Type mismatch",
            TypeCheckerError::TypeMismatchWithOrigin { .. } => "Type mismatch",
            TypeCheckerError::ComplexTypeMismatch { .. } => "Type mismatch",
            TypeCheckerError::ComplexTypeMismatchWithOrigins { .. } => "Type mismatch",
            TypeCheckerError::InvalidReturnOutsideFunction { .. } => "Invalid return statement",
            TypeCheckerError::MissingReturnStatement { .. } => "Missing return statement",
            TypeCheckerError::AssignmentToCapturedVariable { .. } => {
                "Cannot assign to captured variable"
            }
            TypeCheckerError::UndefinedType { .. } => "Undefined type",
            TypeCheckerError::TypeHasNoFields { .. } => "Type has no fields",
            TypeCheckerError::UndefinedField { .. } => "Undefined field",
            TypeCheckerError::StructOutsideOfGlobalScope { .. } => "Invalid struct definition",
            TypeCheckerError::UndefinedMethod { .. } => "Undefined method",
            TypeCheckerError::StaticMethodOnInstance { .. } => {
                "Cannot call static method on instance"
            }
            TypeCheckerError::Redeclaration { .. } => "Redeclaration",
            TypeCheckerError::AssignmentToImmutableBinding { .. } => "Cannot assign to binding",
            TypeCheckerError::MissingInterfaceMethods { .. } => "Interface not implemented",
            TypeCheckerError::InterfaceMethodTypeMismatch { .. } => {
                "Interface method type mismatch"
            }
            TypeCheckerError::UncoveredPattern { .. } => "Uncovered pattern",
            TypeCheckerError::TooManyArguments { .. } => "Too many arguments",
            TypeCheckerError::DuplicateArgument { .. } => "Duplicate argument",
            TypeCheckerError::UndefinedParameter { .. } => "Undefined parameter",
            TypeCheckerError::MissingArgument { .. } => "Missing argument",
            TypeCheckerError::PositionalArgumentAfterNamed { .. } => "Invalid argument order",
            TypeCheckerError::InvalidTupleIndex { .. } => "Invalid tuple index",
            TypeCheckerError::UnreachablePattern { .. } => "Unreachable pattern",
            TypeCheckerError::InvalidIsUsage { .. } => "Invalid 'is' operator usage",
            TypeCheckerError::CannotInferType { .. } => "Cannot infer type",
            TypeCheckerError::InvalidGenericSpecification { .. } => "Invalid generic specification",
            TypeCheckerError::GenericCountMismatch { .. } => "Generic count mismatch",
        }
    }
}

impl TypeCheckerWarning {
    pub fn create_report<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        let offset = self.span().start;
        let mut report = Report::build(ReportKind::Warning, source_id, offset);

        match self {
            TypeCheckerWarning::UnusedBinding { name, span } => {
                report = report
                    .with_message(format!("Unused binding '{}'", name))
                    .with_label(
                        Label::new((source_id, span.to_range()))
                            .with_message(format!("Binding '{}' is never used", name))
                            .with_color(Color::Yellow),
                    )
                    .with_help("Consider using '_' to explicitly ignore this value");
            }
            TypeCheckerWarning::SafeAccessOnNonOptional { span } => {
                report = report
                    .with_message("Safe access operator on non-optional type")
                    .with_label(
                        Label::new((source_id, span.to_range()))
                            .with_message("This type is not optional, safe access has no effect")
                            .with_color(Color::Yellow),
                    )
                    .with_help("Remove the '?' operator as it's not needed here");
            }
            TypeCheckerWarning::RedundantForceUnwrap { span } => {
                report = report
                    .with_message("Force unwrap on non-optional type")
                    .with_label(
                        Label::new((source_id, span.to_range()))
                            .with_message("This type is not optional, force unwrap has no effect")
                            .with_color(Color::Yellow),
                    )
                    .with_help("Remove the '!' operator as it's not needed here");
            }
            TypeCheckerWarning::ShadowedVariable {
                name,
                span,
                original_span,
            } => {
                report = report
                    .with_message(format!("Variable '{}' shadows existing binding", name))
                    .with_labels(vec![
                        Label::new((source_id, span.to_range()))
                            .with_message(format!("'{}' is redeclared here", name))
                            .with_color(Color::Yellow),
                        Label::new((source_id, original_span.to_range()))
                            .with_message(format!("Previous declaration of '{}'", name))
                            .with_color(Color::Blue),
                    ]);
            }
            TypeCheckerWarning::UnreachableCode { span } => {
                report = report
                    .with_message("Unreachable code detected")
                    .with_label(
                        Label::new((source_id, span.to_range()))
                            .with_message("This code will never be executed")
                            .with_color(Color::Yellow),
                    )
                    .with_help("Code after a return statement is unreachable");
            }
        }

        report.finish()
    }

    pub fn span(&self) -> Span {
        match self {
            TypeCheckerWarning::UnusedBinding { span, .. } => *span,
            TypeCheckerWarning::SafeAccessOnNonOptional { span } => *span,
            TypeCheckerWarning::RedundantForceUnwrap { span } => *span,
            TypeCheckerWarning::ShadowedVariable { span, .. } => *span,
            TypeCheckerWarning::UnreachableCode { span } => *span,
        }
    }

    pub fn message(&self) -> String {
        match self {
            TypeCheckerWarning::UnusedBinding { name, .. } => {
                format!("Unused binding '{}'", name)
            }
            TypeCheckerWarning::SafeAccessOnNonOptional { .. } => {
                "Safe access operator on non-optional type".to_string()
            }
            TypeCheckerWarning::RedundantForceUnwrap { .. } => {
                "Force unwrap on non-optional type".to_string()
            }
            TypeCheckerWarning::ShadowedVariable { name, .. } => {
                format!("Variable '{}' shadows existing binding", name)
            }
            TypeCheckerWarning::UnreachableCode { .. } => "Unreachable code detected".to_string(),
        }
    }
}

impl std::fmt::Display for TypeCheckerWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}
