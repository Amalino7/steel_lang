use crate::diagnostics::builder::DiagBuilder;
use crate::diagnostics::{Diagnostic, DiagnosticContext, IntoDiagnostic};
use crate::resolver::collisions::Collision;
use crate::scanner::Span;
use std::fmt::{self, Display, Formatter};
use std::path::PathBuf;

/// A problem with one import. `span` is the module path in the importing file.
#[derive(Debug)]
pub enum ResolverError {
    Io {
        path: PathBuf,
        error: std::io::Error,
        span: Span,
    },
    ModuleNotFound {
        module_name: String,
        searched_paths: Vec<PathBuf>,
        span: Span,
    },
    CyclicDependency {
        cycle: Vec<String>,
        span: Span,
    },
    ReservedModuleName {
        module_name: String,
        span: Span,
        reason: String,
    },
    /// Both `binding.steel` and `binding/index.steel` exist.
    AmbiguousModule {
        module_name: String,
        paths: Vec<PathBuf>,
        span: Span,
    },
    /// The module binding differs in case from the file on disk (`Math` for `math.steel`).
    ModuleNameCase {
        module_name: String,
        on_disk: String,
        span: Span,
    },
}

impl ResolverError {
    pub fn io(path: impl Into<PathBuf>, error: std::io::Error, span: Span) -> Self {
        Self::Io {
            path: path.into(),
            error,
            span,
        }
    }

    pub fn module_not_found(
        module_name: impl Into<String>,
        searched_paths: Vec<PathBuf>,
        span: Span,
    ) -> Self {
        Self::ModuleNotFound {
            module_name: module_name.into(),
            searched_paths,
            span,
        }
    }

    pub fn cyclic_dependency(cycle: Vec<String>, span: Span) -> Self {
        Self::CyclicDependency { cycle, span }
    }

    pub fn reserved_module_name(
        module_name: impl Into<String>,
        span: Span,
        reason: impl Into<String>,
    ) -> Self {
        Self::ReservedModuleName {
            module_name: module_name.into(),
            reason: reason.into(),
            span,
        }
    }

    pub fn message(&self) -> String {
        match self {
            ResolverError::Io { path, error, .. } => {
                format!("IO error at '{}': {}", path.display(), error)
            }
            ResolverError::ModuleNotFound {
                module_name,
                searched_paths,
                ..
            } => {
                if searched_paths.is_empty() {
                    format!("Module '{}' not found", module_name)
                } else {
                    let searched = searched_paths
                        .iter()
                        .map(|p| format!("'{}'", p.display()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!(
                        "Module '{}' not found. Looked in: {}",
                        module_name, searched
                    )
                }
            }
            ResolverError::CyclicDependency { cycle, .. } => {
                format!("Cyclic dependency detected: {}", cycle.join(" -> "))
            }
            ResolverError::ReservedModuleName { module_name, .. } => {
                format!("Module binding '{}' is reserved", module_name)
            }
            ResolverError::AmbiguousModule { module_name, .. } => {
                format!("Module '{}' is ambiguous", module_name)
            }
            ResolverError::ModuleNameCase {
                module_name,
                on_disk,
                ..
            } => {
                format!(
                    "Module '{}' differs in case from '{}'",
                    module_name, on_disk
                )
            }
        }
    }
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl IntoDiagnostic for ResolverError {
    fn into_diagnostic(self, _: &DiagnosticContext) -> Diagnostic {
        match self {
            ResolverError::ModuleNotFound {
                module_name,
                searched_paths,
                span,
            } => {
                let mut diag = DiagBuilder::error(
                    span,
                    "E080",
                    "Module not found",
                    format!("module '{module_name}' not found"),
                );
                if !searched_paths.is_empty() {
                    let searched = searched_paths
                        .iter()
                        .map(|p| format!("'{}'", p.display()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    diag = diag.with_note(format!("looked in: {searched}"));
                }
                diag.build()
            }
            ResolverError::Io { path, error, span } => DiagBuilder::error(
                span,
                "E081",
                "Could not read module",
                format!("cannot read '{}': {}", path.display(), error),
            )
            .build(),
            ResolverError::CyclicDependency { cycle, span } => {
                DiagBuilder::error(span, "E082", "Cyclic import", "this import closes a cycle")
                    .with_note(format!("import cycle: {}", cycle.join(" -> ")))
                    .build()
            }
            ResolverError::ReservedModuleName {
                module_name,
                span,
                reason,
            } => DiagBuilder::error(
                span,
                "E083",
                "Reserved module binding",
                format!("'{module_name}' contains reserved components. {reason}"),
            )
            .with_note("the standard library is always in scope; it cannot be imported")
            .build(),
            ResolverError::AmbiguousModule {
                module_name,
                paths,
                span,
            } => {
                let found = paths
                    .iter()
                    .map(|p| format!("'{}'", p.display()))
                    .collect::<Vec<_>>()
                    .join(", ");
                DiagBuilder::error(
                    span,
                    "E086",
                    "Ambiguous module",
                    format!("module '{module_name}' matches more than one file"),
                )
                .with_note(format!("found: {found}"))
                .build()
            }
            ResolverError::ModuleNameCase {
                module_name,
                on_disk,
                span,
            } => DiagBuilder::error(
                span,
                "E088",
                "Module binding differs in case",
                format!("module '{module_name}' does not match the case of its file"),
            )
            .with_note(format!("on disk it is '{on_disk}'"))
            .build(),
        }
    }
}

/// The entry module cannot be loaded. There is no import site to point at and no source to render, so this is
/// not a diagnostic.
#[derive(Debug)]
pub enum EntryError {
    Io {
        path: PathBuf,
        error: std::io::Error,
    },
    /// A source map without its entry module.
    MissingEntry {
        name: String,
    },
    /// The entry module is called `std`, which is reserved for the standard library.
    ReservedName {
        name: String,
    },
    PathCollisions {
        collisions: Vec<Collision>,
    },
}

impl EntryError {
    pub fn message(&self) -> String {
        match self {
            EntryError::Io { path, error } => {
                format!("cannot read '{}': {}", path.display(), error)
            }
            EntryError::MissingEntry { name } => {
                format!("entry module '{}' is not in the source map", name)
            }
            EntryError::ReservedName { name } => {
                format!("'{}' is reserved for the standard library", name)
            }
            EntryError::PathCollisions { collisions } => {
                collisions.iter().map(|c| c.to_string()).collect()
            }
        }
    }
}

impl Display for EntryError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}

/// Why a module's source could not be loaded
#[derive(Debug)]
pub(crate) enum LoadError {
    NotFound {
        searched_paths: Vec<PathBuf>,
    },
    Io {
        path: PathBuf,
        error: std::io::Error,
    },
    Ambiguous {
        paths: Vec<PathBuf>,
    },
    NameCase {
        on_disk: String,
    },
}

impl LoadError {
    pub fn into_resolver_err(self, module_name: String, mod_span: Span) -> ResolverError {
        match self {
            LoadError::NotFound { searched_paths } => {
                ResolverError::module_not_found(module_name, searched_paths, mod_span)
            }
            LoadError::Io { path, error } => ResolverError::Io {
                path,
                error,
                span: mod_span,
            },
            LoadError::Ambiguous { paths } => ResolverError::AmbiguousModule {
                module_name,
                paths,
                span: mod_span,
            },
            LoadError::NameCase { on_disk } => ResolverError::ModuleNameCase {
                module_name,
                on_disk,
                span: mod_span,
            },
        }
    }
}
