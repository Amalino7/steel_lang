use crate::scanner::Span;
use ariadne::{Color, Config, IndexType, Label, Report, ReportKind};
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
    UnreachablePattern {
        span: Span,
        message: String,
    },
}

impl TypeCheckerWarning {
    pub fn create_report<'a>(&self, source_id: &'a str) -> Report<'a, (&'a str, Range<usize>)> {
        let offset = self.span().start;
        let mut report = Report::build(ReportKind::Warning, source_id, offset)
            .with_config(Config::default().with_index_type(IndexType::Byte));

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
            TypeCheckerWarning::UnreachablePattern { span, message } => {
                report = report.with_message("Unreachable pattern").with_label(
                    Label::new((source_id, span.to_range()))
                        .with_message(message.as_str())
                        .with_color(Color::Yellow),
                );
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
            TypeCheckerWarning::UnreachablePattern { span, .. } => *span,
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
            TypeCheckerWarning::UnreachablePattern { message, .. } => message.clone(),
        }
    }
}
