use crate::scanner::{Span, Token};
use crate::typechecker::core::error::{BindingError, TypeCheckerError};
use crate::typechecker::core::types::NameTypeId;
use crate::typechecker::scope::binding::{Binding, Origin};
use crate::typechecker::scope::variables::DeclarationKind;
use crate::typechecker::system::MethodId;
use crate::typechecker::{Symbol, TypeChecker};

impl<'src> TypeChecker<'src> {
    /// Inherent methods first, then extensions in scope.
    pub(crate) fn lookup_method(
        &self,
        type_id: NameTypeId,
        name: &str,
    ) -> Option<Binding<MethodId>> {
        self.sys
            .lookup_inherent_method(type_id, name)
            .or_else(|| self.type_scopes.lookup_method(type_id, name))
    }

    /// All method names callable on the type (for suggestions).
    pub(crate) fn method_names_for_type(&self, type_id: NameTypeId) -> Vec<Symbol> {
        let mut names = self.sys.inherent_method_names(type_id);
        names.extend(self.type_scopes.get_methods_for_type(type_id));
        // Private methods of other modules are never suggested.
        names.retain(|name| {
            self.lookup_method(type_id, name)
                .is_none_or(|binding| self.is_method_visible(binding))
        });
        names
    }

    /// Orders methods by where they are defined, so conflicts are reported deterministically
    /// regardless of hash order.
    pub(crate) fn sorted_by_origin<'a>(
        &self,
        methods: impl Iterator<Item = (NameTypeId, &'a Symbol, Binding<MethodId>)>,
    ) -> Vec<(NameTypeId, &'a Symbol, Binding<MethodId>)> {
        let mut methods: Vec<_> = methods.collect();
        methods.sort_by_key(|(_, _, binding)| {
            let origin = self.sys.get_method_info(binding.target).origin;
            (origin.file_id, origin.start)
        });
        methods
    }

    pub(crate) fn extension_conflict(
        &mut self,
        res: Result<(), Binding<MethodId>>,
        type_id: NameTypeId,
        name: &str,
        second: Span,
        span: Span,
    ) {
        let Err(old) = res else { return };
        let first = self.sys.get_method_info(old.target).origin;
        if first.file_id == second.file_id {
            self.redeclaration_method(Err(old), name, span);
        } else {
            self.report(TypeCheckerError::ConflictingExtension {
                method_name: name.into(),
                type_name: self.sys.get_name(type_id),
                span,
                first,
                second,
            });
        }
    }

    pub(crate) fn redeclaration_method(
        &mut self,
        res: Result<(), Binding<MethodId>>,
        name: &str,
        new_location: Span,
    ) {
        if let Err(old) = res {
            let definition = self.sys.get_method_info(old.target).origin;
            self.report(TypeCheckerError::Binding(BindingError::Redeclaration {
                name: name.into(),
                span: new_location,
                original: old.origin.span().unwrap_or(definition),
                original_kind: DeclarationKind::Method,
            }));
        }
    }

    /// Public methods are visible everywhere, private ones only in their own module. An imported
    /// extension is callable: being bound in this module's table is the access.
    fn is_method_visible(&self, binding: Binding<MethodId>) -> bool {
        match binding.origin {
            Origin::Defined { span, public } => public || span.file_id == self.file_id,
            Origin::Imported { .. } | Origin::Builtin => true,
        }
    }

    pub(crate) fn check_method_access(&mut self, binding: Binding<MethodId>, name: &Token) {
        if self.is_method_visible(binding) {
            return;
        }
        let info = self.sys.get_method_info(binding.target);
        let error = TypeCheckerError::PrivateMethod {
            method_name: name.lexeme.into(),
            module: self.module_graph.module_name_of_file(info.origin.file_id),
            span: name.span,
            definition: info.origin,
        };
        self.report(error);
    }
}
