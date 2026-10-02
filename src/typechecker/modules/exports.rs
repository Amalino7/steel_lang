use crate::resolver::Exports;
use crate::typechecker::TypeChecker;

impl<'ctx> TypeChecker<'ctx> {
    pub fn get_exports(&mut self) -> Exports {
        let mut exports = Exports::new();
        exports.types = self.type_scopes.export_types();
        let mut extensions = self.type_scopes.export_methods();
        // Re-export guard: else importers would also get the extensions this module imported.
        extensions.retain(|id| self.sys.get_method_info(id).origin.file_id == self.file_id);
        exports.extensions = extensions;
        exports.vars = self.scopes.export_vars();
        exports
    }
}
