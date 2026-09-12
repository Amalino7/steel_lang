pub mod error;
pub mod resolution;

pub use error::ResolverError;
pub use resolution::ModuleResolver;

use std::path::PathBuf;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleGraph {
    pub modules: Vec<ModuleInfo>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleInfo {
    pub id: ModuleId,
    pub name: String,
    pub path: PathBuf,
    pub source: String,
    pub dependencies: Vec<ModuleId>,
}
