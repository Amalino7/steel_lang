use crate::diagnostics::{Diagnostic, LabelKind, Level};
use crate::resolver::{FileId, ModuleGraph, ModuleInfo};
use ariadne::{Cache, Color, Config, Label, Report, ReportKind, sources};
use std::ops::Range;

pub fn render<'a>(
    diag: &Diagnostic,
    config: Config,
    module_graph: &'a ModuleGraph,
) -> Report<'a, (String, Range<usize>)> {
    let kind = match diag.level {
        Level::Error => ReportKind::Error,
        Level::Warning => ReportKind::Warning,
    };

    let primary_color = match diag.level {
        Level::Error => Color::Red,
        Level::Warning => Color::Yellow,
    };

    let mut b = Report::build(
        kind,
        file_id_to_name(diag.primary_span.file_id, module_graph),
        diag.primary_span.start,
    )
    .with_config(config)
    .with_code(diag.code)
    .with_message(diag.title.clone());

    b = b.with_label(
        Label::new((
            file_id_to_name(diag.primary_span.file_id, module_graph),
            diag.primary_span.to_range(),
        ))
        .with_message(&diag.message)
        .with_color(primary_color),
    );

    for l in &diag.labels {
        let color = match l.kind {
            LabelKind::Origin => Color::Blue,
            LabelKind::Secondary => Color::Yellow,
        };
        b = b.with_label(
            Label::new((
                file_id_to_name(l.span.file_id, module_graph),
                l.span.to_range(),
            ))
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

pub fn cache(graph: &ModuleGraph) -> impl Cache<String> {
    sources(
        graph
            .modules
            .iter()
            .map(|info| (module_label(info), &info.source)),
    )
}

/// How a module is named in rendered diagnostics. It uses the module_path.steel and <std> for standard library.
fn module_label(module: &ModuleInfo) -> String {
    if module.is_std {
        return "<std>".to_owned();
    }
    let mut name = module.name.clone();
    name.push_str(".steel");
    name
}

fn file_id_to_name(file_id: FileId, module_graph: &ModuleGraph) -> String {
    module_graph.file_to_module_id.get(&file_id).map_or_else(
        || "<unknown>".to_string(),
        |id| module_label(&module_graph.modules[id.0 as usize]),
    )
}
