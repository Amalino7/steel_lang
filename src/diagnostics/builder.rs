use crate::diagnostics::{DiagLabel, Diagnostic, LabelKind, Level};
use crate::scanner::Span;

pub struct DiagBuilder {
    inner: Diagnostic,
}

impl DiagBuilder {
    pub fn error(
        span: Span,
        code: &'static str,
        title: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        let inner = Diagnostic {
            primary_span: span,
            code,
            title: title.into(),
            message: message.into(),
            level: Level::Error,
            helps: vec![],
            labels: vec![],
            notes: vec![],
        };
        DiagBuilder { inner }
    }

    pub fn warn(
        span: Span,
        code: &'static str,
        title: impl Into<String>,
        message: impl Into<String>,
    ) -> Self {
        let inner = Diagnostic {
            primary_span: span,
            code,
            title: title.into(),
            message: message.into(),
            level: Level::Warning,
            helps: vec![],
            labels: vec![],
            notes: vec![],
        };
        DiagBuilder { inner }
    }

    pub fn with_origin(mut self, span: Span, msg: impl Into<String>) -> Self {
        self.inner.labels.push(DiagLabel {
            span,
            kind: LabelKind::Origin,
            message: msg.into(),
        });
        self
    }

    pub fn with_optional_origin(mut self, span: Option<Span>, msg: impl Into<String>) -> Self {
        if let Some(span) = span {
            self.inner.labels.push(DiagLabel {
                span,
                kind: LabelKind::Origin,
                message: msg.into(),
            });
        }
        self
    }

    pub fn with_secondary(mut self, span: Span, msg: impl Into<String>) -> Self {
        self.inner.labels.push(DiagLabel {
            span,
            kind: LabelKind::Secondary,
            message: msg.into(),
        });
        self
    }

    pub fn with_help(mut self, msg: impl Into<String>) -> Self {
        self.inner.helps.push(msg.into());
        self
    }
    pub fn with_note(mut self, msg: impl Into<String>) -> Self {
        self.inner.notes.push(msg.into());
        self
    }
    pub fn with_optional_note(self, msg: Option<String>) -> Self {
        match msg {
            Some(msg) => self.with_note(msg),
            None => self,
        }
    }
    pub fn with_suggestion(self, suggestions: &[String]) -> Self {
        if suggestions.is_empty() {
            self
        } else {
            self.with_help(format!("Did you mean '{}'?", suggestions.join("', '")))
        }
    }
    pub fn build(self) -> Diagnostic {
        self.inner
    }
}
