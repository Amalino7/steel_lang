pub mod ariadne;
pub mod builder;
pub mod emitter;

use crate::scanner::Span;

pub struct Diagnostic {
    level: Level,
    code: &'static str,
    title: &'static str,
    primary_span: Span,
    message: String,
    labels: Vec<DiagLabel>,
    notes: Vec<String>,
    helps: Vec<String>,
}

pub enum Level {
    Error,
    Warning,
}

pub struct DiagLabel {
    span: Span,
    kind: LabelKind,
    message: String,
}

pub enum LabelKind {
    Origin,
    Secondary,
}
