pub mod error;
pub mod new_pipeline;
pub mod resolution;

pub use error::ResolverError;
pub use resolution::ModuleResolver;
use std::collections::HashMap;

use crate::typechecker::Symbol;
use crate::typechecker::core::types::NameTypeId;
use crate::typechecker::method_table::MethodTable;
use std::path::PathBuf;

use crate::typechecker::scope::variables::VariableContext;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

pub use crate::scanner::FileId;

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
    pub dependencies: Vec<ModuleId>,
    pub exports: Exports,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Exports {
    pub types: HashMap<Symbol, NameTypeId>,
    pub extensions: MethodTable,
    pub vars: HashMap<Symbol, VariableContext>,
}

impl Exports {
    pub fn new() -> Self {
        Exports {
            types: Default::default(),
            extensions: Default::default(),
            vars: Default::default(),
        }
    }
}

impl Default for Exports {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleGraph {
    pub fn new() -> Self {
        ModuleGraph {
            modules: vec![],
            name_to_id_map: HashMap::new(),
            file_to_module_id: HashMap::new(),
        }
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
