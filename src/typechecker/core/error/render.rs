use crate::typechecker::core::error::report::{Diagnostic, ReportBuilder};
use crate::typechecker::core::error::{
    context_defined_at_message, context_short_message, kind_note, mismatch_label_message, BindingError, CallParamError,
    CallParamKind, DuplicateDefinition, GenericError, Operand,
    TypeCheckerError, TypeRequirement,
};
use ariadne::Report;
use std::ops::Range;

impl TypeCheckerError {
    pub fn create_report<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        match self {
            TypeCheckerError::Duplicate(d) => d.render(source_id),
            TypeCheckerError::CallParam(c) => c.render(source_id),
            TypeCheckerError::Call(c) => c.render(source_id),
            TypeCheckerError::Generic(g) => g.render(source_id),
            TypeCheckerError::Binding(b) => b.render(source_id),
            TypeCheckerError::TypeMismatch {
                mismatch,
                context,
                primary_span,
                defined_at,
            } => {
                let primary_msg = if mismatch.precise.is_some() {
                    format!(
                        "expected '{}', found '{}'",
                        mismatch.expected, mismatch.found
                    )
                } else {
                    mismatch_label_message(mismatch)
                };
                let mut builder = ReportBuilder::error(
                    source_id,
                    *primary_span,
                    self.code(),
                    context_short_message(context),
                )
                .primary(*primary_span, primary_msg);
                if let Some(origin) = defined_at {
                    builder = builder.secondary(*origin, context_defined_at_message(context));
                }
                if let Some(detail) = &mismatch.precise {
                    builder = builder.note(kind_note(&mismatch.kind, detail));
                }
                builder.finish()
            }

            TypeCheckerError::OperatorConstraint {
                operator,
                operand,
                found,
                requirement,
                span,
            } => {
                let operand_str = match operand {
                    Operand::Lhs => "left operand",
                    Operand::Rhs => "right operand",
                    Operand::Unary => "operand",
                };
                let req_str = match requirement {
                    TypeRequirement::Exact(ty) => ty.to_string(),
                    TypeRequirement::Structural(s) => s.to_string(),
                };
                ReportBuilder::error(
                    source_id,
                    *span,
                    self.code(),
                    format!(
                        "Operator '{}' cannot be applied to type '{}'",
                        operator, found
                    ),
                )
                .primary(
                    *span,
                    format!(
                        "The {} must be '{}', found '{}'",
                        operand_str, req_str, found
                    ),
                )
                .finish()
            }

            TypeCheckerError::MissingReturnStatement {
                span,
                fn_span,
                fn_name,
            } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Function '{}' is missing a return statement", fn_name),
            )
            .primary(*span, "Implicitly returns () here")
            .origin(*fn_span, format!("'{}' declared here", fn_name))
            .note("Functions with a return type must return a value on all paths.")
            .finish(),

            TypeCheckerError::MissingInterfaceMethods {
                missing_methods,
                interface,
                span,
                interface_origin,
            } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Methods needed to implement '{}' are missing", interface),
            )
            .primary(
                *span,
                format!("Missing methods: {}", missing_methods.join(", ")),
            )
            .secondary(
                *interface_origin,
                format!("Interface '{}' declared here", interface),
            )
            .finish(),

            TypeCheckerError::InterfaceMethodTypeMismatch {
                method_name,
                interface,
                type_mismatch,
                span,
                interface_origin,
            } => {
                let mut builder = ReportBuilder::error(
                    source_id,
                    *span,
                    self.code(),
                    format!(
                        "Method '{}' does not satisfy interface '{}'",
                        method_name, interface
                    ),
                )
                .primary(
                    *span,
                    format!(
                        "Expected '{}' but found '{}'",
                        type_mismatch.expected, type_mismatch.found
                    ),
                )
                .secondary(
                    *interface_origin,
                    format!(
                        "'{}' is declared with type '{}' in interface '{}'",
                        method_name, type_mismatch.expected, interface
                    ),
                );
                if let Some(detail) = &type_mismatch.precise {
                    builder = builder.note(kind_note(&type_mismatch.kind, detail));
                }
                builder.finish()
            }

            TypeCheckerError::UndefinedField {
                struct_name,
                field_name,
                span,
                struct_origin,
                suggestions,
            } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Undefined field '{}'", field_name),
            )
            .primary(
                *span,
                format!(
                    "Field '{}' does not exist on struct '{}'",
                    field_name, struct_name
                ),
            )
            .optional_origin(
                *struct_origin,
                format!("Struct '{}' defined here", struct_name),
            )
            .suggest(suggestions)
            .finish(),

            TypeCheckerError::UndefinedMethod {
                span,
                found,
                method_name,
                type_origin,
                suggestions,
            } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Undefined method '{}'", method_name),
            )
            .primary(
                *span,
                format!(
                    "Method '{}' does not exist on type '{}'",
                    method_name, found
                ),
            )
            .optional_origin(*type_origin, format!("Type '{}' defined here", found))
            .suggest(suggestions)
            .finish(),

            TypeCheckerError::UndefinedVariable {
                name,
                span,
                suggestions,
            } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Undefined variable '{}'", name),
            )
            .primary(
                *span,
                format!("Variable '{}' is not defined in this scope", name),
            )
            .suggest(suggestions)
            .finish(),

            TypeCheckerError::InvalidOperandTypes {
                operator,
                left,
                right,
                span,
                help,
            } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Invalid operand types for '{}'", operator),
            )
            .primary(
                *span,
                format!(
                    "Operator '{}' cannot be applied to types '{}' and '{}'.",
                    operator, left, right
                ),
            )
            .help(*help)
            .finish(),

            // Generic fallback
            err => ReportBuilder::error(source_id, err.span(), err.code(), err.title())
                .primary(err.span(), err.message())
                .finish(),
        }
    }
}

impl DuplicateDefinition {
    pub(crate) fn render<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        ReportBuilder::error(
            source_id,
            self.span,
            self.code(),
            self.kind.report_message(&self.name),
        )
        .origin(self.original, self.kind.origin_label(&self.name))
        .primary(self.span, self.kind.primary_label(&self.name))
        .finish()
    }
}

impl CallParamError {
    pub(crate) fn render<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        let primary_msg = match self.kind {
            CallParamKind::Missing => format!("Missing required argument '{}'", self.param_name),
            CallParamKind::Undefined => format!("Parameter '{}' does not exist", self.param_name),
        };
        ReportBuilder::error(source_id, self.span, self.code(), self.title())
            .primary(self.span, primary_msg)
            .optional_origin(self.callee_origin, "Callee declared here")
            .finish()
    }
}

impl BindingError {
    pub(crate) fn render<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        match self {
            BindingError::Redeclaration {
                name,
                span,
                original,
                original_kind,
            } => {
                let kind_str = original_kind.as_str();
                ReportBuilder::error(
                    source_id,
                    *span,
                    self.code(),
                    format!("Redeclaration of {} '{}'", kind_str, name),
                )
                .primary(*span, format!("Cannot redeclare {} '{}'", kind_str, name))
                .secondary(
                    *original,
                    format!("{} '{}' originally declared here", kind_str, name),
                )
                .finish()
            }
            BindingError::Immutable {
                kind,
                name,
                span,
                definition_span,
            } => {
                let kind_str = kind.as_str();
                ReportBuilder::error(
                    source_id,
                    *span,
                    self.code(),
                    format!("Cannot assign to {} '{}'", kind_str, name),
                )
                .primary(
                    *span,
                    format!("'{}' is a {}, not a variable", name, kind_str),
                )
                .origin(
                    *definition_span,
                    format!("{} '{}' declared here", kind_str, name),
                )
                .finish()
            }
            BindingError::Captured {
                name,
                span,
                capture_origin,
            } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Cannot assign to captured variable '{}'", name),
            )
            .origin(*capture_origin, "Captured happens here.")
            .primary(*span, "Assignment happens here.")
            .finish(),
        }
    }
}

impl GenericError {
    pub(crate) fn render<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        match self {
            GenericError::CannotInfer {
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
                ReportBuilder::error(source_id, *span, self.code(), "Cannot infer type")
                    .primary(*span, label_msg)
                    .help(
                        "Add a type annotation or specify generics explicitly using .<Type> syntax",
                    )
                    .finish()
            }
            GenericError::CountMismatch {
                span,
                found,
                expected,
                type_name,
            } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                format!("Generic count mismatch for '{}'", type_name),
            )
            .primary(
                *span,
                format!(
                    "Expected {} generic argument(s) but found {} for '{}'",
                    expected, found, type_name
                ),
            )
            .finish(),
            GenericError::InvalidSpecification { span, message } => ReportBuilder::error(
                source_id,
                *span,
                self.code(),
                "Invalid generic specification",
            )
            .primary(*span, message.as_str())
            .finish(),
        }
    }
}
