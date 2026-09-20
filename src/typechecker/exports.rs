use crate::resolver::Exports;
use crate::typechecker::TypeChecker;

impl<'ctx> TypeChecker<'ctx> {
    pub fn get_exports(&mut self) -> Exports {
        let mut exports = Exports::new();
        exports.types = self.type_scopes.export_types();
        exports.methods = self.type_scopes.export_methods();
        exports.vars = self.scopes.export_vars();
        exports
    }
}
