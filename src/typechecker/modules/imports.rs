use crate::parser::ast::{ImportSegment, ImportType, Stmt};
use crate::resolver::Exports;
use crate::scanner::{FileId, Span, Token};
use crate::typechecker::core::error::{Recoverable, TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::core::types::Type;
use crate::typechecker::scope::variables::{Declaration, VariableContext};
use crate::typechecker::similarity::find_similar;
use crate::typechecker::{Symbol, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub fn imports(&mut self, ast: &[Stmt<'src>]) {
        for stmt in ast {
            if let Stmt::Import(import) = stmt {
                self.handle_import(None, &import.segment)
            }
        }
    }

    fn handle_import(&mut self, prefix: Option<&str>, import: &ImportSegment) {
        let mut path_segments = import.path.iter().map(|p| p.lexeme).collect::<Vec<_>>();

        if let Some(prefix) = prefix {
            path_segments.insert(0, prefix);
        }

        let path = path_segments.join(".");

        match &import.import_type {
            ImportType::Simple { terminator } => {
                self.import_named(&path, terminator, terminator);
            }
            ImportType::Alias { terminator, alias } => {
                self.import_named(&path, terminator, alias);
            }
            ImportType::Group { options } => {
                for option in options {
                    self.handle_import(Some(&path), option);
                }
            }
            ImportType::Wildcard => {
                let Some(module_info) = self.module_graph.module_by_name(&path) else {
                    return;
                };
                let import_span = import
                    .path
                    .first()
                    .zip(import.path.last())
                    .map(|(first, last)| first.span.merge(last.span));
                self.import_all(&module_info.exports, module_info.file_id, import_span);
            }
        }
    }
    fn import_named(&mut self, path: &str, terminator: &Token, target: &Token) {
        enum ImportStatus {
            Success,
            Private(Span),
            NotReexported,
            NotFound,
        }

        let Some(module_info) = self.module_graph.module_by_name(path) else {
            return;
        };
        let exports = &module_info.exports;
        let m_file = module_info.file_id;

        let mut status = ImportStatus::NotFound;

        if let Some(&id) = exports.types.get(terminator.lexeme) {
            // Re-export guard: `is_public` is the type's own flag, so imported types would pass.
            let local_export = self.sys.defining_file(id) == m_file;
            let has_extensions = exports.extensions.has_methods_for(id);

            if local_export && self.sys.is_public(id) {
                let res = self.type_scopes.declare_global(target.lexeme.into(), id);
                self.redeclaration_type(res, target.span);
                status = ImportStatus::Success;
            } else if local_export {
                status = ImportStatus::Private(self.sys.get_origin(id).unwrap_or_default());
            } else if has_extensions && self.sys.is_public(id) {
                status = ImportStatus::Success;
                if target.lexeme != terminator.lexeme {
                    self.warn(TypeCheckerWarning::UnboundExtensionAlias {
                        alias: target.lexeme.to_string(),
                        name: terminator.lexeme.to_string(),
                        span: target.span,
                    });
                }
                let methods = exports.extensions.methods_of(id);
                for (_, name, method_id) in self.sorted_by_origin(methods.map(|(n, m)| (id, n, m)))
                {
                    let res = self.type_scopes.declare_method(id, name.clone(), method_id);
                    let origin = self.sys.get_method_info(method_id).origin;
                    // Reported at the import that brings in the second method.
                    self.extension_conflict(res, id, name, origin, target.span);
                }
            } else {
                status = ImportStatus::NotReexported;
            }
        }

        if let Some(ctx) = exports.vars.get(terminator.lexeme) {
            // Re-export guard: an imported copy; distinguishes "not exported" from "private".
            if ctx.span.file_id != m_file {
                status = ImportStatus::NotReexported
            } else if !ctx.is_public {
                status = ImportStatus::Private(ctx.span);
            } else {
                status = ImportStatus::Success;
                self.scopes
                    .declare_existing(&VariableContext {
                        name: target.lexeme.into(),
                        type_info: ctx.type_info.clone(),
                        original_type: ctx.original_type.clone(),
                        ..*ctx
                    })
                    .ok_or_report(&mut self.errors);
            }
        }

        if matches!(status, ImportStatus::Success) {
            return;
        }

        let _ = self
            .scopes
            .declare(Declaration::variable(
                target.lexeme.into(),
                Type::Error,
                target.span,
            ))
            .ok_or_report(&mut self.errors);
        // The type does exist, so bind its name too: otherwise every use in a type position
        // would add a misleading "type not defined" error on top of the import error.
        if let Some(&id) = exports.types.get(terminator.lexeme)
            && self.type_scopes.lookup_type(target.lexeme).is_none()
        {
            let _ = self.type_scopes.declare_global(target.lexeme.into(), id);
        }

        if let ImportStatus::Private(definition) = status {
            self.report(TypeCheckerError::PrivateImport {
                name: terminator.lexeme.into(),
                module: module_info.name.clone(),
                span: terminator.span,
                definition,
            });
            return;
        }

        let visible_names = self.public_names(exports, m_file);
        let candidates = visible_names.iter().map(|name| name.as_ref());
        let suggestions = find_similar(terminator.lexeme, candidates, 3);
        let note = if matches!(status, ImportStatus::NotReexported) {
            Some(format!(
                "`{}` imports `{}` but does not export it; re-exports are not supported",
                module_info.name, terminator.lexeme
            ))
        } else {
            None
        };

        self.report(TypeCheckerError::ImportNotFound {
            name: terminator.lexeme.into(),
            span: terminator.span,
            suggestions,
            note,
        });
    }

    /// Names a module defines and marks `public` for suggestions
    fn public_names(&self, exports: &Exports, m_file: FileId) -> Vec<Symbol> {
        let types = exports
            .types
            .iter()
            // Re-export guard: skip types the module imported.
            .filter(|(_, id)| self.sys.defining_file(**id) == m_file && self.sys.is_public(**id))
            .map(|(name, _)| name.clone());
        let vars = exports
            .vars
            .iter()
            // Re-export guard: imported vars are already private; kept in step with the types.
            .filter(|(_, ctx)| ctx.span.file_id == m_file && ctx.is_public)
            .map(|(name, _)| name.clone());
        types.chain(vars).collect()
    }

    /// Used by wildcard imports and prelude. Brings all public items from the module in scope.
    ///
    /// `import_span` is where extension clashes are reported; `None` (the prelude) falls back to
    /// the clashing method's own definition.
    pub fn import_all(&mut self, exports: &Exports, m_file: FileId, import_span: Option<Span>) {
        for (name, id) in exports.types.iter() {
            // Re-export guard: skip types the module imported.
            if self.sys.defining_file(*id) != m_file || !self.sys.is_public(*id) {
                continue;
            }
            let res = self.type_scopes.declare_global(name.clone(), *id);
            self.redeclaration_type(res, self.sys.get_origin(*id).unwrap_or_default())
        }

        for (id, method_name, method_id) in self.sorted_by_origin(exports.extensions.iter()) {
            let res = self
                .type_scopes
                .declare_method(id, method_name.clone(), method_id);
            let origin = self.sys.get_method_info(method_id).origin;
            self.extension_conflict(res, id, method_name, origin, import_span.unwrap_or(origin));
        }

        for var in exports.vars.values() {
            // Re-export guard: imported vars are already private; kept in step with the types.
            if var.span.file_id == m_file && var.is_public {
                self.scopes
                    .declare_existing(var)
                    .ok_or_report(&mut self.errors);
            }
        }
    }
}
