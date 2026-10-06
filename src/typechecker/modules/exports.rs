use crate::scanner::Span;
use crate::typechecker::TypeChecker;
use crate::typechecker::core::types::{NameTypeId, Symbol};
use crate::typechecker::method_table::MethodTable;
use crate::typechecker::scope::binding::{Binding, Origin};
use crate::typechecker::scope::variables::VariableContext;
use std::collections::HashMap;

/// The surface a module offers to its importers.
///
/// `types` and `vars` keep every top-level binding of the module (private, imported and builtin
/// ones too) so importers can tell "private" from "not re-exported" and from "missing"; only the
/// lookups and the `exported_*` iterators say what may be imported. `extensions` holds only the
/// exported ones and is read-only outside this module.
#[derive(Debug, Clone, PartialEq)]
pub struct Exports {
    types: HashMap<Symbol, Binding<NameTypeId>>,
    extensions: MethodTable,
    vars: HashMap<Symbol, VariableContext>,
}

#[derive(PartialEq)]
pub enum TypeLookup<'ctx> {
    Found {
        binding: &'ctx Binding<NameTypeId>,
        extension_only: bool,
    },
    NotFound,
    Private {
        binding: &'ctx Binding<NameTypeId>,
        is_reexport: bool,
        span: Span,
    },
}

impl TypeLookup<'_> {
    pub fn has_succeeded(&self) -> bool {
        matches!(self, TypeLookup::Found { .. })
    }
}

#[derive(PartialEq)]
pub enum VarLookup<'ctx> {
    Found(&'ctx VariableContext),
    Private {
        var: &'ctx VariableContext,
        is_reexport: bool,
        span: Span,
    },
    NotFound,
}

impl VarLookup<'_> {
    pub fn has_succeeded(&self) -> bool {
        matches!(self, VarLookup::Found { .. })
    }
}

impl Exports {
    pub fn new() -> Self {
        Exports {
            types: Default::default(),
            extensions: Default::default(),
            vars: Default::default(),
        }
    }

    pub fn lookup_type(&self, name: &str) -> TypeLookup<'_> {
        let Some(binding) = self.types.get(name) else {
            return TypeLookup::NotFound;
        };

        match binding.origin {
            Origin::Defined { public: true, .. } => TypeLookup::Found {
                binding,
                extension_only: false,
            },
            Origin::Defined { span, .. } => TypeLookup::Private {
                binding,
                span,
                is_reexport: false,
            },
            Origin::Imported { .. } | Origin::Builtin
                if self.extensions.has_methods_for(binding.target) =>
            {
                TypeLookup::Found {
                    binding,
                    extension_only: true,
                }
            }
            Origin::Imported { import_span: span } => TypeLookup::Private {
                binding,
                span,
                is_reexport: true,
            },
            Origin::Builtin => TypeLookup::NotFound,
        }
    }

    pub fn lookup_var(&self, name: &str) -> VarLookup<'_> {
        let Some(var) = self.vars.get(name) else {
            return VarLookup::NotFound;
        };
        match var.origin {
            Origin::Defined { public: true, .. } => VarLookup::Found(var),
            Origin::Defined { span, .. } => VarLookup::Private {
                var,
                span,
                is_reexport: false,
            },
            Origin::Imported { import_span: span } => VarLookup::Private {
                span,
                var,
                is_reexport: true,
            },
            Origin::Builtin => VarLookup::NotFound,
        }
    }

    /// Exported types, in no particular order.
    pub fn exported_types(&self) -> impl Iterator<Item = (&Symbol, &Binding<NameTypeId>)> {
        self.types
            .iter()
            .filter(|(_, binding)| binding.origin.is_exported())
    }

    /// Exported functions and globals, in no particular order.
    pub fn exported_vars(&self) -> impl Iterator<Item = (&Symbol, &VariableContext)> {
        self.vars.iter().filter(|(_, var)| var.origin.is_exported())
    }

    /// Extensions this module defines and exports.
    pub fn extensions(&self) -> &MethodTable {
        &self.extensions
    }
}

impl Default for Exports {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ctx> TypeChecker<'ctx> {
    pub fn get_exports(&mut self) -> Exports {
        let mut extensions = self.type_scopes.export_methods();
        // Neither private nor imported extensions are offered to importers.
        extensions.retain(|binding| binding.origin.is_exported());
        Exports {
            types: self.type_scopes.export_types(),
            extensions,
            vars: self.scopes.export_vars(),
        }
    }
}
