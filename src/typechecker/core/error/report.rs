use crate::scanner::Span;
use ariadne::{Color, Config, IndexType, Label, Report, ReportKind};
use std::ops::Range;

/// Common interface for all type-checker error diagnostics.
///
/// - `title` is the short category header shown at the top of the report.
/// - `message` is a longer description used for `Display` and fallback label text.
pub trait Diagnostic {
    fn span(&self) -> Span;
    fn code(&self) -> &'static str;
    fn title(&self) -> &'static str;
    fn message(&self) -> String;
}

/// Convenience builder over `ariadne`'s report API.
///
/// Encodes the project's label colour conventions:
/// - **primary** (`primary`) → red — the main site being flagged.
/// - **origin** (`origin`) → blue — where something was defined or declared.
/// - **secondary** (`secondary`) → yellow — related context (e.g. call site, previous use).
///
/// Call `finish()` to produce the final `Report`.
pub struct ReportBuilder<'a> {
    source_id: &'a str,
    inner: ariadne::ReportBuilder<'a, (&'a str, Range<usize>)>,
}

impl<'a> ReportBuilder<'a> {
    /// Start an error report positioned at `span`.
    pub fn error(
        source_id: &'a str,
        span: Span,
        code: &'static str,
        title: impl Into<String>,
    ) -> Self {
        let inner = Report::build(ReportKind::Error, source_id, span.start)
            .with_config(Config::default().with_index_type(IndexType::Byte))
            .with_code(code)
            .with_message(title.into());
        Self { source_id, inner }
    }

    /// Add a red label — the main site being highlighted.
    pub fn primary(mut self, span: Span, msg: impl Into<String>) -> Self {
        self.inner = self.inner.with_label(
            Label::new((self.source_id, span.to_range()))
                .with_message(msg.into())
                .with_color(Color::Red),
        );
        self
    }

    /// Add a blue label — where something was defined or declared.
    pub fn origin(mut self, span: Span, msg: impl Into<String>) -> Self {
        self.inner = self.inner.with_label(
            Label::new((self.source_id, span.to_range()))
                .with_message(msg.into())
                .with_color(Color::Blue),
        );
        self
    }

    /// Add a yellow label — related context without implying a definition site.
    pub fn secondary(mut self, span: Span, msg: impl Into<String>) -> Self {
        self.inner = self.inner.with_label(
            Label::new((self.source_id, span.to_range()))
                .with_message(msg.into())
                .with_color(Color::Yellow),
        );
        self
    }

    /// Add an origin label only when `span` is `Some`. No-op otherwise.
    pub fn optional_origin(self, span: Option<Span>, msg: impl Into<String>) -> Self {
        match span {
            Some(s) => self.origin(s, msg),
            None => self,
        }
    }

    /// Add a help message.
    pub fn help(mut self, msg: impl Into<String>) -> Self {
        self.inner = self.inner.with_help(msg.into());
        self
    }

    /// Add a note.
    pub fn note(mut self, msg: impl Into<String>) -> Self {
        self.inner = self.inner.with_note(msg.into());
        self
    }

    /// Add a "Did you mean?" help line if `suggestions` is non-empty.
    pub fn suggest(self, suggestions: &[String]) -> Self {
        if suggestions.is_empty() {
            self
        } else {
            self.help(format!("Did you mean '{}'?", suggestions.join("', '")))
        }
    }

    pub fn finish(self) -> Report<'a, (&'a str, Range<usize>)> {
        self.inner.finish()
    }
}
