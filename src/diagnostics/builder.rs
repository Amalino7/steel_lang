use crate::diagnostics::{DiagLabel, Diagnostic, LabelKind, Level};
use crate::scanner::Span;

pub struct DiagBuilder {
    level: Level,
    code: &'static str,
    title: &'static str,
    message: String,
    primary_span: Span,
    labels: Vec<DiagLabel>,
    notes: Vec<String>,
    helps: Vec<String>,
}

impl DiagBuilder {
    pub fn error(
        span: Span,
        code: &'static str,
        title: &'static str,
        message: impl Into<String>,
    ) -> Self {
        DiagBuilder {
            primary_span: span,
            code,
            title: title.into(),
            message: message.into(),
            level: Level::Error,
            helps: vec![],
            labels: vec![],
            notes: vec![],
        }
    }

    pub fn origin(mut self, span: Span, msg: impl Into<String>) -> Self {
        self.labels.push(DiagLabel {
            span,
            kind: LabelKind::Origin,
            message: msg.into(),
        });
        self
    }

    pub fn secondary(mut self, span: Span, msg: impl Into<String>) -> Self {
        self.labels.push(DiagLabel {
            span,
            kind: LabelKind::Secondary,
            message: msg.into(),
        });
        self
    }

    pub fn help(mut self, msg: impl Into<String>) -> Self {
        self.helps.push(msg.into());
        self
    }
    pub fn note(mut self, msg: impl Into<String>) -> Self {
        self.notes.push(msg.into());
        self
    }
    pub fn suggest(self, suggestions: &[String]) -> Self {
        if suggestions.is_empty() {
            self
        } else {
            self.help(format!("Did you mean '{}'?", suggestions.join("', '")))
        }
    }
    pub fn build(self) -> Diagnostic {
        Diagnostic {
            level: self.level,
            code: self.code,
            title: self.code,
            primary_span: self.primary_span,
            message: self.message,
            labels: self.labels,
            notes: self.notes,
            helps: self.helps,
        }
    }
}
