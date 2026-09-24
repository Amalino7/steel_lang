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
        }
    }
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message())
    }
}
