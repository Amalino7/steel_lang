pub mod collisions;
pub mod error;
pub mod resolution;

pub use error::{EntryError, ResolverError};
pub use resolution::ModuleResolver;
use std::collections::HashMap;

use std::path::PathBuf;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

pub use crate::scanner::{FileId, PRELUDE_FILE};

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleGraph {
    pub modules: Vec<ModuleInfo>,
    pub name_to_id_map: HashMap<String, ModuleId>,
    pub file_to_module_id: HashMap<FileId, ModuleId>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleInfo {
    pub id: ModuleId,
    pub file_id: FileId,
    pub name: String,
    pub path: PathBuf,
    pub source: String,
    pub imports: Vec<ModuleId>,
    pub is_std: bool,
}

/// The reserved name of the standard library module.
pub const STD_MODULE: &str = "std";

impl ModuleGraph {
    pub fn new() -> Self {
        ModuleGraph {
            modules: vec![],
            name_to_id_map: HashMap::new(),
            file_to_module_id: HashMap::new(),
        }
    }

    /// The standard library module, if the graph has one (`--no-stdlib` and most unit tests have none).
    pub fn std_module(&self) -> Option<&ModuleInfo> {
        self.modules.iter().find(|module| module.is_std)
    }

    pub fn module_by_name(&self, name: &str) -> Option<&ModuleInfo> {
        let id = self.name_to_id_map.get(name)?;
        self.modules.get(id.0 as usize)
    }

    /// Name of the module whose source file has the given id.
    pub fn module_name_of_file(&self, file_id: FileId) -> String {
        self.file_to_module_id
            .get(&file_id)
            .and_then(|id| self.modules.get(id.0 as usize))
            .map(|module| module.name.clone())
            .unwrap_or_default()
    }
}

impl Default for ModuleGraph {
    fn default() -> Self {
        Self::new()
    }
}
