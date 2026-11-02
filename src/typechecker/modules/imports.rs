use crate::parser::ast::{Ident, Import, ImportKind, ImportTree};
use crate::resolver::ModuleInfo;
use crate::scanner::Span;
use crate::typechecker::Exports;
use crate::typechecker::core::error::{Recoverable, TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::core::types::{NameTypeId, PrimitiveTypeId, Type};
use crate::typechecker::modules::exports::{TypeLookup, VarLookup};
use crate::typechecker::scope::binding::{Binding, Origin};
use crate::typechecker::scope::variables::{Declaration, VariableContext};
use crate::typechecker::similarity::find_similar;
use crate::typechecker::{Symbol, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub fn imports(&mut self, imports: &[Import<'src>]) {
        for import in imports {
            self.handle_import(None, &import.tree)
        }
    }

    fn handle_import(&mut self, prefix: Option<&str>, import: &ImportTree) {
        let mut path_segments = import.path.iter().map(|p| p.name).collect::<Vec<_>>();

        if let Some(prefix) = prefix {
            path_segments.insert(0, prefix);
        }

        let path = path_segments.join("/");

        match &import.kind {
            ImportKind::Item { name } => {
                self.import_named(&path, name, name);
            }
            ImportKind::Alias { name, alias } => {
                self.import_named(&path, name, alias);
            }
            ImportKind::Group(group) => {
                for tree in &group.items {
                    self.handle_import(Some(&path), tree);
                }
            }
            ImportKind::Wildcard(star) => {
                let Some((_, exports)) = self.import_target(&path) else {
                    return;
                };
                self.import_all(exports, *star);
            }
        }
    }
    /// The module imported as `path` by the current module, with its surface.
    /// None for failed imports.
    fn import_target(&self, path: &str) -> Option<(&'src ModuleInfo, &'src Exports)> {
        let current_mod_id = *self.module_graph.file_to_module_id.get(&self.file_id)?;
        let current_mod = self.module_graph.modules.get(current_mod_id.0 as usize)?;
        let target_mod = self.module_graph.module_by_name(path)?;

        // Prevents modules not imported due to cycles.
        if !current_mod.imports.contains(&target_mod.id) {
            return None;
        }

        let exports = self.surfaces.get(target_mod.id.0 as usize)?.as_ref()?;

        Some((target_mod, exports))
    }

    fn import_named(&mut self, path: &str, terminator: &Ident, target: &Ident) {
        let Some((module_info, exports)) = self.import_target(path) else {
            self.bind_unavailable(target);
            return;
        };

        let type_lookup = exports.lookup_type(terminator.name);
        if let TypeLookup::Found {
            binding,
            extension_only,
        } = type_lookup
        {
            if !extension_only {
                let imported = Binding::new(
                    binding.target,
                    Origin::Imported {
                        import_span: target.span,
                    },
                );
                let res = self
                    .type_scopes
                    .declare_global(target.name.into(), imported);
                self.redeclaration_type(res, target.name, target.span);
            }
            self.import_extensions(binding.target, exports, extension_only, terminator, target)
        }

        let var_lookup = exports.lookup_var(terminator.name);

        if let VarLookup::Found(var) = var_lookup {
            self.declare_imported(var, target.name.into(), target.span);
        }

        if type_lookup.has_succeeded() || var_lookup.has_succeeded() {
            return;
        }
        // Error cases
        let private_err = |span, reexport: bool| {
            let note = reexport.then(|| {
                format!(
                    "`{}` imports `{}` but does not export it; re-exports are not supported",
                    module_info.name, terminator.name
                )
            });

            TypeCheckerError::PrivateImport {
                name: terminator.name.into(),
                module: module_info.name.clone(),
                span: terminator.span,
                definition: span,
                note,
            }
        };

        if let TypeLookup::Private {
            binding,
            is_reexport,
            span,
        } = type_lookup
        {
            let imported = Binding::new(
                binding.target,
                Origin::Imported {
                    import_span: target.span,
                },
            );
            let _ = self
                .type_scopes
                .declare_global(target.name.into(), imported);
            self.import_extensions(binding.target, exports, false, terminator, target);
            self.report(private_err(span, is_reexport));
        }

        if let VarLookup::Private {
            var,
            is_reexport,
            span,
        } = var_lookup
        {
            self.declare_imported(var, target.name.into(), target.span);
            self.report(private_err(span, is_reexport));
        }

        if TypeLookup::NotFound == type_lookup && VarLookup::NotFound == var_lookup {
            self.bind_unavailable(target);

            let visible_names = Self::public_names(exports);
            let candidates = visible_names.iter().map(|name| name.as_ref());
            let suggestions = find_similar(terminator.name, candidates, 3);
            self.report(TypeCheckerError::ImportNotFound {
                name: terminator.name.into(),
                span: terminator.span,
                suggestions,
            });
        }
    }

    /// Adds fallback bindings for missing items and unavailable modules.
    fn bind_unavailable(&mut self, target: &Ident) {
        let _ = self
            .scopes
            .declare(Declaration::variable(
                target.name.into(),
                Type::Error,
                target.span,
            ))
            .ok_or_report(&mut self.errors);

        let binding = Binding::new(
            PrimitiveTypeId::Error.into(),
            Origin::Imported {
                import_span: target.span,
            },
        );
        let _ = self.type_scopes.declare_global(target.name.into(), binding);
    }

    /// Imports the extensions a module defines on an external type.
    fn import_extensions(
        &mut self,
        id: NameTypeId,
        exports: &Exports,
        is_extension_only: bool,
        terminator: &Ident,
        target: &Ident,
    ) {
        if target.name != terminator.name && is_extension_only {
            self.warn(TypeCheckerWarning::UnboundExtensionAlias {
                alias: target.name.to_string(),
                name: terminator.name.to_string(),
                span: target.span,
            });
        }
        let methods = exports.extensions().methods_of(id);
        for (_, name, method) in self.sorted_by_origin(methods.map(|(n, m)| (id, n, m))) {
            let imported = Binding::new(
                method.target,
                Origin::Imported {
                    import_span: target.span,
                },
            );
            let res = self.type_scopes.declare_method(id, name.clone(), imported);
            let origin = self.sys.get_method_info(method.target).origin;
            // Reported at the import that brings in the second method.
            self.extension_conflict(res, id, name, origin, target.span);
        }
    }

    /// Names a module defines and marks `public` for suggestions
    fn public_names(exports: &Exports) -> Vec<Symbol> {
        let types = exports.exported_types().map(|(name, _)| name.clone());
        let vars = exports.exported_vars().map(|(name, _)| name.clone());
        let mut names: Vec<Symbol> = types.chain(vars).collect();
        // Hash order must not leak into the suggestions.
        names.sort();
        names
    }

    /// Binds the exported `var` as `binding`, imported at `span`.
    fn declare_imported(&mut self, var: &VariableContext, name: Symbol, span: Span) {
        let imported = VariableContext {
            name,
            origin: Origin::Imported { import_span: span },
            ..var.clone()
        };
        let _ = self
            .scopes
            .declare_existing(imported)
            .ok_or_report(&mut self.errors);
    }

    /// Used by wildcard imports and prelude. Brings all public items from the module in scope.
    pub fn import_all(&mut self, exports: &Exports, import_span: Span) {
        let mut types: Vec<_> = exports.exported_types().collect();
        types.sort_by(|a, b| a.0.cmp(b.0));

        for (name, binding) in types {
            let imported = Binding::new(binding.target, Origin::Imported { import_span });
            let res = self.type_scopes.declare_global(name.clone(), imported);
            self.redeclaration_type(res, name, import_span)
        }

        for (id, method_name, method) in self.sorted_by_origin(exports.extensions().iter()) {
            let origin = self.sys.get_method_info(method.target).origin;
            let imported = Binding::new(method.target, Origin::Imported { import_span });
            let res = self
                .type_scopes
                .declare_method(id, method_name.clone(), imported);
            self.extension_conflict(res, id, method_name, origin, import_span);
        }

        let mut vars: Vec<_> = exports.exported_vars().collect();
        vars.sort_by(|a, b| a.0.cmp(b.0));
        for (name, var) in vars {
            self.declare_imported(var, name.clone(), import_span);
        }
    }
}
