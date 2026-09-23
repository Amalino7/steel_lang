use crate::diagnostics::builder::DiagBuilder;
use crate::diagnostics::{DiagnosticContext, IntoDiagnostic};
use crate::typechecker::core::error::{
    BindingError, CallError, CallParamKind, GenericError, Operand, TypeCheckerError,
    TypeRequirement, context_defined_at_message, context_short_message, kind_note,
    mismatch_label_message,
};

impl IntoDiagnostic for TypeCheckerError {
    fn into_diagnostic(self, ctx: &DiagnosticContext) -> crate::diagnostics::Diagnostic {
        let code = self.code();

        match self {
            TypeCheckerError::SelfOutsideOfImpl { span } => DiagBuilder::error(
                span,
                code,
                "Cannot use 'self' and 'Self' outside of an implementation block.",
                "Invalid use of 'self'",
            ),
            TypeCheckerError::UndefinedType {
                name,
                span,
                message,
            } => DiagBuilder::error(
                span,
                code,
                format!("Type '{name}' is not defined in this scope."),
                format!("This type does not exist in scope. {message}"),
            ),
            TypeCheckerError::UndefinedVariable {
                name,
                span,
                suggestions,
            } => DiagBuilder::error(
                span,
                code,
                format!("Variable '{name}' is not defined in this scope."),
                "This variable does not exist in scope.",
            )
                .with_suggestion(&suggestions),
            TypeCheckerError::CalleeIsNotCallable { found, span } => DiagBuilder::error(
                span,
                code,
                format!(
                    "Callee '{}' is not callable.",
                    found.display_type(ctx.type_system)
                ),
                "This callee is not callable.",
            ),
            TypeCheckerError::TypeMismatch { mismatch, context, primary_span, defined_at } => {
                let mut builder = DiagBuilder::error(primary_span,
                                   code,
                                   format!("{}. Found {}, but expected {}",context_short_message(&context), mismatch.found.display_type(ctx.type_system), mismatch.expected.display_type(ctx.type_system)),
                                   mismatch_label_message(&mismatch, ctx),
                ).with_optional_origin(defined_at, context_defined_at_message(&context));

                if let Some(detail) = &mismatch.precise {
                    builder = builder.with_note(kind_note(&mismatch.kind, detail, ctx));
                }
                builder
            }
            TypeCheckerError::InterfaceMethodTypeMismatch { method_name, interface, type_mismatch, span, interface_origin } => {
                let mut builder = DiagBuilder::error(
                    span,
                    code,
                    format!("Method '{}' does not satisfy interface {}.", method_name, interface),
                    mismatch_label_message(&type_mismatch, ctx),
                ).with_origin(interface_origin, format!(
                    "'{}' is declared with type '{}' in interface '{}'",
                    method_name, type_mismatch.expected.display_type(ctx.type_system), interface
                ));
                if let Some(detail) = &type_mismatch.precise {
                    builder = builder.with_note(kind_note(&type_mismatch.kind, detail, ctx));
                }
                builder
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
                    TypeRequirement::Exact(ty) => ty.display_type(ctx.type_system),
                    TypeRequirement::Structural(s) => s.to_string(),
                };

                DiagBuilder::error(
                    span,
                    code,
                    format!(
                        "Operator constraint violation: Operator '{operator}' cannot be applied to type '{}'"
                        , found.display_type(ctx.type_system)),
                    format!(
                        "The {} must be '{}',but found '{}'",
                        operand_str, req_str, found.display_type(ctx.type_system)
                    ),
                )
            }
            TypeCheckerError::InvalidReturnOutsideFunction { span } => DiagBuilder::error(
                span,
                code,
                "Return statement outside of function.",
                "Invalid return statement.",
            ),
            TypeCheckerError::MissingReturnStatement {
                fn_span,
                fn_name,
                span,
            } => DiagBuilder::error(
                span,
                code,
                format!("Function {fn_name} is missing mandatory return statement."),
                "Implicitly returns void here",
            )
                .with_origin(fn_span, format!("'{}' declared here", fn_name))
                .with_note("Functions with a return type must return a value on all paths."),
            TypeCheckerError::TypeHasNoFields { found, span } => DiagBuilder::error(
                span,
                code,
                format!(
                    "Type '{}' has no fields.",
                    found.display_type(ctx.type_system)
                ),
                "This type has no fields.",
            ),
            TypeCheckerError::UndefinedField {
                struct_name,
                field_name,
                span,
                struct_origin,
                suggestions,
            } => {
                DiagBuilder::error(
                    span,
                    code,
                    format!("Type '{}' has no field '{}'.", struct_name, field_name),
                    format!("This type has no field named {}.", field_name),
                )
                    .with_optional_origin(struct_origin, "Type defined here.")
                    .with_suggestion(&suggestions)
            }
            TypeCheckerError::NonGlobalDeclaration { name, kind, span } => DiagBuilder::error(
                span,
                code,
                format!("Non-global declaration of {}", kind),
                format!("Cannot declare a {} named '{}' here.", kind, name),
            ),
            TypeCheckerError::StaticMethodOnInstance { method_name, span } => DiagBuilder::error(
                span,
                code,
                format!(
                    "Static method '{}' cannot be called on an instance.",
                    method_name
                ),
                "Static method called here.",
            ),
            TypeCheckerError::MissingInterfaceMethods {
                missing_methods,
                interface,
                span,
                interface_origin,
            } => DiagBuilder::error(
                span,
                code,
                format!(
                    "Missing methods: {:?} for interface {}",
                    missing_methods, interface
                ),
                "Methods required here.",
            )
                .with_origin(interface_origin, "Interface defined here."),
            TypeCheckerError::UncoveredPattern { variant, span } => DiagBuilder::error(
                span,
                code,
                format!("Uncovered pattern: {}", variant),
                "Uncovered pattern here.",
            ),
            TypeCheckerError::InvalidTupleIndex {
                tuple_type,
                index,
                span,
            } => DiagBuilder::error(
                span,
                code,
                format!(
                    "Invalid tuple index: {} for tuple type {}",
                    index,
                    tuple_type.display_type(ctx.type_system)
                ),
                "Invalid index here.",
            ),
            TypeCheckerError::InvalidIsUsage { span, message } => {
                DiagBuilder::error(span, code, message, "Invalid usage of `is` here.")
            }
            TypeCheckerError::InvalidOperandTypes(err) => {
                DiagBuilder::error(
                    err.span,
                    code,
                    format!("Invalid operand types: {} and {} for operator {}",
                            err.left.display_type(ctx.type_system), err.right.display_type(ctx.type_system), err.operator),
                    "Invalid operand types here.",
                ).with_help(err.help)
            }
            TypeCheckerError::PrimitiveTypeShadowing { .. } => {
                todo!()
            }
            TypeCheckerError::Duplicate(err) => {
                DiagBuilder::error(
                    err.span,
                    code,
                    format!("Duplicate definition of {} {}", err.kind.noun(), err.name),
                    format!("Duplicate {} here.", err.kind.noun()),
                ).with_origin(err.original, "First defined here.")
            }
            TypeCheckerError::CallParam(err) => {
                let title = match err.kind {
                    CallParamKind::Missing => format!("Missing required argument '{}'", err.param_name),
                    CallParamKind::Undefined => format!("Parameter '{}' does not exist", err.param_name),
                };

                DiagBuilder::error(
                    err.span,
                    code,
                    title,
                    "Call occurs here.",
                ).with_optional_origin(err.callee_origin, "Callee declared here.")
            }
            TypeCheckerError::Call(err) => {
                match err {
                    CallError::TooMany { expected, found, span, callee, callee_origin } => {
                        DiagBuilder::error(
                            span,
                            code,
                            "Too many arguments passed to callee.",
                            format!("Expected {} arguments, but found {}.", expected, found),
                        ).with_optional_origin(callee_origin, "Callee defined here.")
                            .with_secondary(callee, "This is the callee.")
                    }
                    CallError::DuplicateArgument { name, span } => {
                        DiagBuilder::error(
                            span,
                            code,
                            "Duplicate argument name.",
                            format!("Argument '{}' is passed here.", name),
                        ) // TODO add original place
                    }
                    CallError::PositionalAfterNamed { message, span } => {
                        DiagBuilder::error(
                            span,
                            code,
                            "Positional argument after named argument.",
                            message,
                        )
                    }
                }
            }
            TypeCheckerError::Generic(err) => {
                match err {
                    GenericError::CannotInfer { span, uninferred_generics } => {
                        let label_msg = if uninferred_generics.is_empty() {
                            "Cannot infer the type here".to_string()
                        } else {
                            format!(
                                "Cannot infer generic type parameter(s): {}",
                                uninferred_generics.join(", ")
                            )
                        };
                        DiagBuilder::error(
                            span,
                            code,
                            "Could not infer all generic types.",
                            label_msg,
                        )
                            .with_help(
                                "Add a type annotation or specify generics explicitly using .<Type> syntax",
                            )
                    }
                    GenericError::CountMismatch { span, found, expected, type_name } => {
                        DiagBuilder::error(
                            span,
                            code,
                            format!("Wrong number of generic arguments for type {}.", type_name),
                            format!("Expected {} generic arguments, but found {} here.", expected, found),
                        )
                    }
                    GenericError::InvalidSpecification { span, message } => {
                        DiagBuilder::error(
                            span,
                            code,
                            "Invalid generic specification.",
                            message,
                        )
                    }
                }
            }
            TypeCheckerError::Binding(err) => {
                match err {
                    BindingError::Redeclaration { name, span, original, original_kind } => {
                        DiagBuilder::error(
                            span,
                            code,
                            format!("Redeclaration of {} {}.", original_kind.as_str(), name),
                            format!("{} redeclared here.", name),
                        )
                            .with_origin(original, format!("{} was originally declared here.", original_kind.as_str()))
                    }
                    BindingError::Immutable { kind, name, span, definition_span } => {
                        DiagBuilder::error(
                            span,
                            code,
                            format!("Cannot assign to {} {}.", kind.as_str(), name),
                            format!("{} is immutable.", name),
                        )
                            .with_origin(definition_span, format!("{} '{}' declared here", kind.as_str(), name))
                    }
                    BindingError::Captured { name, span, capture_origin } => {
                        DiagBuilder::error(
                            span,
                            code,
                            format!("Cannot assign to captured variable '{}'", name),
                            "Assignment happens here.",
                        )
                            .with_origin(capture_origin, "Capture happens here.")
                    }
                }
            }
            TypeCheckerError::UndefinedMethod(err) => {
                DiagBuilder::error(
                    err.span,
                    code,
                    format!("Could not find method named '{}' on type {}", err.method_name, err.found.display_type(ctx.type_system)),
                    "Could not find this method.",
                ).with_optional_origin(err.type_origin, "Type defined here.")
                    .with_suggestion(&err.suggestions)
            }
            TypeCheckerError::ImportNotFound { name, span, suggestions } => {
                DiagBuilder::error(
                    span,
                    code,
                    format!("Could not find Item named '{}' to import.", name),
                    "Could not find this Item.",
                )
                    .with_suggestion(&suggestions)
            }
        }
            .build()
    }
}
