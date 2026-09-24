pub mod ariadne;
pub mod builder;
pub mod emitter;

use crate::scanner::Span;
use crate::typechecker::system::TypeSystem;

#[derive(Debug)]
pub struct Diagnostic {
    level: Level,
    code: &'static str,
    title: String,
    primary_span: Span,
    message: String,
    labels: Vec<DiagLabel>,
    notes: Vec<String>,
    helps: Vec<String>,
}

#[derive(Debug)]
pub enum Level {
    Error,
    Warning,
}

#[derive(Debug)]
pub struct DiagLabel {
    span: Span,
    kind: LabelKind,
    message: String,
}

#[derive(Debug)]
pub enum LabelKind {
    Origin,
    Secondary,
}

pub struct DiagnosticContext<'ctx> {
    pub type_system: &'ctx TypeSystem,
}

pub trait IntoDiagnostic {
    fn into_diagnostic(self, ctx: &DiagnosticContext) -> Diagnostic;
}
