pub mod error;
pub mod new_pipeline;
pub mod resolution;

pub use error::ResolverError;
pub use resolution::ModuleResolver;
use std::collections::HashMap;

use crate::typechecker::Symbol;
use crate::typechecker::core::types::NameTypeId;
use crate::typechecker::system::MethodId;
use std::path::PathBuf;

use crate::typechecker::scope::variables::VariableContext;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleGraph {
    pub modules: Vec<ModuleInfo>,
    pub name_to_id_map: HashMap<String, ModuleId>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleInfo {
    pub id: ModuleId,
    pub name: String,
    pub path: PathBuf,
    pub source: String,
    pub dependencies: Vec<ModuleId>,
    pub exports: Exports,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Exports {
    pub types: HashMap<Symbol, NameTypeId>,
    pub methods: HashMap<(NameTypeId, Symbol), MethodId>,
    pub vars: HashMap<Symbol, VariableContext>,
}

impl Exports {
    pub fn new() -> Self {
        Exports {
            types: Default::default(),
            methods: Default::default(),
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
        }
    }
}

impl Default for ModuleGraph {
    fn default() -> Self {
        Self::new()
    }
}
