use std::fmt::{self, Display, Formatter};
use std::path::PathBuf;

#[derive(Debug)]
pub enum ResolverError {
    Io {
        path: PathBuf,
        error: std::io::Error,
    },
    ModuleNotFound {
        module_name: String,
        searched_paths: Vec<PathBuf>,
    },
    CyclicDependency {
        cycle: Vec<String>,
    },
    ParseError {
        module_name: String,
        path: PathBuf,
        errors: Vec<String>,
    },
}

impl ResolverError {
    pub fn io(path: impl Into<PathBuf>, error: std::io::Error) -> Self {
        Self::Io {
            path: path.into(),
            error,
        }
    }

    pub fn module_not_found(module_name: impl Into<String>, searched_paths: Vec<PathBuf>) -> Self {
        Self::ModuleNotFound {
            module_name: module_name.into(),
            searched_paths,
        }
    }

    pub fn cyclic_dependency(cycle: Vec<String>) -> Self {
        Self::CyclicDependency { cycle }
    }

    pub fn parse_error(
        module_name: impl Into<String>,
        path: impl Into<PathBuf>,
        errors: Vec<String>,
    ) -> Self {
        Self::ParseError {
            module_name: module_name.into(),
            path: path.into(),
            errors,
        }
    }

    pub fn message(&self) -> String {
        match self {
            ResolverError::Io { path, error } => {
                if path.as_os_str().is_empty() {
                    format!("IO error: {}", error)
                } else {
                    format!("IO error at '{}': {}", path.display(), error)
                }
            }
            ResolverError::ModuleNotFound {
                module_name,
                searched_paths,
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
            ResolverError::CyclicDependency { cycle } => {
                format!("Cyclic dependency detected: {}", cycle.join(" -> "))
            }
            ResolverError::ParseError {
                module_name,
                path,
                errors,
            } => {
                if errors.is_empty() {
                    format!(
                        "Failed to parse module '{}' at '{}'",
                        module_name,
                        path.display()
                    )
                } else {
                    format!(
                        "Failed to parse module '{}' at '{}':\n{}",
                        module_name,
                        path.display(),
                        errors.join("\n")
                    )
                }
            }
        }
    }
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}
