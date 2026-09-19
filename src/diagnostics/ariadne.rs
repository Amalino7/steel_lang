use crate::diagnostics::{Diagnostic, LabelKind, Level};
use ariadne::{Color, Config, Label, Report, ReportKind};

pub fn render<'a>(
    source_id: &'a str,
    diag: &Diagnostic,
    config: Config,
) -> Report<'a, (&'a str, std::ops::Range<usize>)> {
    let kind = match diag.level {
        Level::Error => ReportKind::Error,
        Level::Warning => ReportKind::Warning,
    };

    let primary_color = match diag.level {
        Level::Error => Color::Red,
        Level::Warning => Color::Yellow,
    };

    let mut b = Report::build(kind, source_id, diag.primary_span.start)
        .with_config(config)
        .with_code(diag.code)
        .with_message(diag.title);

    b = b.with_label(
        Label::new((source_id, diag.primary_span.to_range()))
            .with_message(&diag.message)
            .with_color(primary_color),
    );

    for l in &diag.labels {
        let color = match l.kind {
            LabelKind::Origin => Color::Blue,
            LabelKind::Secondary => Color::Yellow,
        };
        b = b.with_label(
            Label::new((source_id, l.span.to_range()))
                .with_message(l.message.clone())
                .with_color(color),
        );
    }
    for n in &diag.notes {
        b = b.with_note(n.clone());
    }
    for h in &diag.helps {
        b = b.with_help(h.clone());
    }
    b.finish()
}
