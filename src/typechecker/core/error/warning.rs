use crate::diagnostics::builder::DiagBuilder;
use crate::diagnostics::{DiagnosticContext, IntoDiagnostic};
use crate::scanner::Span;

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
    UnreachablePattern {
        span: Span,
        message: String,
    },
    /// `import m/T as U;` only brings `m`'s extensions on `T`; the alias binds nothing.
    UnboundExtensionAlias {
        alias: String,
        name: String,
        span: Span,
    },
}

impl IntoDiagnostic for TypeCheckerWarning {
    fn into_diagnostic(self, _: &DiagnosticContext) -> crate::diagnostics::Diagnostic {
        let span = self.span();
        let builder = DiagBuilder::warn(span, "W0001", self.title(), self.message());
        match self {
            TypeCheckerWarning::UnusedBinding { name, .. } => {
                builder.with_help(format!(
                    "Consider removing it or renaming it to '_' or '_{name}' to explicitly ignore this value"
                ))
            }
            TypeCheckerWarning::SafeAccessOnNonOptional { .. } => {
                builder.with_help("Remove the '?' operator as it's not needed here.")
            }
            TypeCheckerWarning::RedundantForceUnwrap { .. } => {
                builder.with_help("Remove the '!' operator as it's not needed here.")
            }
            TypeCheckerWarning::ShadowedVariable { original_span, name,.. } => {
                builder.with_origin(original_span, format!("Previous declaration of '{}'", name) )
            }
            TypeCheckerWarning::UnreachableCode { .. } => {
                builder.with_help("Consider removing this code")
                    .with_note(
                        "Code after a return/continue/break and certain functions is unreachable",
                    )
            }
            TypeCheckerWarning::UnreachablePattern { .. } => {
                builder.with_help("Consider removing this pattern")
            }
            TypeCheckerWarning::UnboundExtensionAlias { name, .. } => {
                builder.with_help(format!(
                    "Import '{name}' from the module that defines it to bind its name; the alias does not rename the extensions."
                ))
            }
        }.build()
    }
}

impl TypeCheckerWarning {
    pub fn title(&self) -> &'static str {
        match self {
            TypeCheckerWarning::UnusedBinding { .. } => "Unused binding",
            TypeCheckerWarning::SafeAccessOnNonOptional { .. } => "Safe access on non-optional",
            TypeCheckerWarning::RedundantForceUnwrap { .. } => "Redundant force unwrap",
            TypeCheckerWarning::ShadowedVariable { .. } => "Shadowed variable",
            TypeCheckerWarning::UnreachableCode { .. } => "Unreachable code",
            TypeCheckerWarning::UnreachablePattern { .. } => "Unreachable pattern",
            TypeCheckerWarning::UnboundExtensionAlias { .. } => "Unbound extension alias",
        }
    }

    pub fn span(&self) -> Span {
        match self {
            TypeCheckerWarning::UnusedBinding { span, .. } => *span,
            TypeCheckerWarning::SafeAccessOnNonOptional { span } => *span,
            TypeCheckerWarning::RedundantForceUnwrap { span } => *span,
            TypeCheckerWarning::ShadowedVariable { span, .. } => *span,
            TypeCheckerWarning::UnreachableCode { span } => *span,
            TypeCheckerWarning::UnreachablePattern { span, .. } => *span,
            TypeCheckerWarning::UnboundExtensionAlias { span, .. } => *span,
        }
    }

    pub fn message(&self) -> String {
        match self {
            TypeCheckerWarning::UnusedBinding { name, .. } => {
                format!("Binding '{}' is never used", name)
            }
            TypeCheckerWarning::SafeAccessOnNonOptional { .. } => {
                "This type is not optional, safe access has no effect".to_string()
            }
            TypeCheckerWarning::RedundantForceUnwrap { .. } => {
                "This type is not optional, force unwrap has no effect".to_string()
            }
            TypeCheckerWarning::ShadowedVariable { name, .. } => {
                format!("'{}' is redeclared here", name)
            }
            TypeCheckerWarning::UnreachableCode { .. } => {
                "This code will never be executed".to_string()
            }
            TypeCheckerWarning::UnreachablePattern { message, .. } => message.clone(),
            TypeCheckerWarning::UnboundExtensionAlias { alias, name, .. } => {
                format!(
                    "Alias '{alias}' is not bound: importing '{name}' here only brings extension methods"
                )
            }
        }
    }
}
