use crate::diagnostics::{Diagnostic, DiagnosticContext, IntoDiagnostic, Level};

pub struct DiagnosticEmitter {
    diagnostics: Vec<Diagnostic>,
    error_count: usize,
    warning_count: usize,
}

impl DiagnosticEmitter {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            error_count: 0,
            warning_count: 0,
        }
    }

    pub fn emit_diag(&mut self, diag: Diagnostic) {
        match diag.level {
            Level::Error => self.error_count += 1,
            Level::Warning => self.warning_count += 1,
        }
        self.diagnostics.push(diag);
    }

    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        self.error_count = 0;
        self.warning_count = 0;
        std::mem::take(&mut self.diagnostics)
    }
}

impl<E: IntoDiagnostic> DiagnosticSink<E> for DiagnosticEmitter {
    fn emit(&mut self, error: E, ctx: &DiagnosticContext<'_>) {
        self.emit_diag(error.into_diagnostic(ctx))
    }
}

pub trait DiagnosticSink<E> {
    fn emit(&mut self, error: E, ctx: &DiagnosticContext<'_>);
}

pub struct RecordingSink<T> {
    errors: Vec<T>,
}

impl<E> DiagnosticSink<E> for RecordingSink<E> {
    fn emit(&mut self, error: E, ctx: &DiagnosticContext<'_>) {
        self.errors.push(error);
    }
}
