use crate::scanner::{Span, Token};
use crate::typechecker::core::error::{
    DuplicateDefinition, DuplicateKind, TypeCheckerError, TypeCheckerWarning,
};
use crate::typechecker::core::types::{GenericTypeId, Type};
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::scope::types::{TypeScopeError, TypeScopeKind};
use crate::typechecker::system::TypeSystem;
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashMap;

pub struct ScopeGuard<'a, 'src> {
    checker: &'a mut TypeChecker<'src>,
}

pub struct TypeScopeGuard<'a, 'src> {
    checker: &'a mut TypeChecker<'src>,
}

impl<'a, 'src> Drop for TypeScopeGuard<'a, 'src> {
    fn drop(&mut self) {
        self.checker.type_scopes.end_type_scope();
    }
}

impl<'a, 'src> std::ops::Deref for TypeScopeGuard<'a, 'src> {
    type Target = TypeChecker<'src>;
    fn deref(&self) -> &Self::Target {
        self.checker
    }
}

impl<'a, 'src> std::ops::DerefMut for TypeScopeGuard<'a, 'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.checker
    }
}

impl<'a, 'src> TypeScopeGuard<'a, 'src> {
    pub fn new_type_params(
        checker: &'a mut TypeChecker<'src>,
        generics: &[Token<'src>],
        ids: &[GenericTypeId],
    ) -> Self {
        let generic_map = Self::create_generic_map(&checker.sys, ids);

        let res = checker
            .type_scopes
            .begin_type_scope(generic_map, None, TypeScopeKind::Type);

        check_duplicate_generics(&mut checker.errors, generics, res);
        Self { checker }
    }
    pub fn new_function(checker: &'a mut TypeChecker<'src>, generics: &[Token<'src>]) -> Self {
        let ids = checker.sys.declare_ids(generics);
        let generic_map = Self::create_generic_map(&checker.sys, &ids);

        let res = checker
            .type_scopes
            .begin_type_scope(generic_map, None, TypeScopeKind::Function);
        check_duplicate_generics(&mut checker.errors, generics, res);
        TypeScopeGuard { checker }
    }

    pub fn old_function(checker: &'a mut TypeChecker<'src>, generics: &[GenericTypeId]) -> Self {
        let generic_map = Self::create_generic_map(&checker.sys, generics);

        let res = checker
            .type_scopes
            .begin_type_scope(generic_map, None, TypeScopeKind::Function);
        TypeScopeGuard { checker }
    }

    pub fn new_impl(checker: &'a mut TypeChecker<'src>, self_ty: Type) -> Self {
        let _ = checker.type_scopes.begin_type_scope(
            HashMap::new(),
            Some(self_ty),
            TypeScopeKind::Impl,
        );
        TypeScopeGuard { checker }
    }
    fn create_generic_map(
        sys: &TypeSystem,
        generic_ids: &[GenericTypeId],
    ) -> HashMap<Symbol, GenericTypeId> {
        let mut map = HashMap::new();
        for &id in generic_ids {
            let name = sys.get_generic(id).name.clone();
            map.insert(name, id);
        }
        map
    }
}
fn check_duplicate_generics(
    errors: &mut Vec<TypeCheckerError>,
    generics: &[Token],
    scope_errors: Result<(), Vec<TypeScopeError>>,
) {
    let Err(scope_errors) = scope_errors else {
        return;
    };
    for err in scope_errors {
        errors.push(TypeCheckerError::Duplicate(DuplicateDefinition {
            kind: DuplicateKind::GenericParam,
            name: err.0.to_string(),
            span: generics[err.1].span,
            original: Default::default(),
        }))
    }
}

impl<'a, 'src> ScopeGuard<'a, 'src> {
    pub fn new(checker: &'a mut TypeChecker<'src>, kind: ScopeKind) -> Self {
        checker.scopes.begin_scope(kind);
        ScopeGuard { checker }
    }

    pub fn new_function(
        checker: &'a mut TypeChecker<'src>,
        return_type: Type,
        origin: Span,
    ) -> Self {
        checker.scopes.begin_function(return_type, origin);
        ScopeGuard { checker }
    }
}
impl<'a, 'src> Drop for ScopeGuard<'a, 'src> {
    fn drop(&mut self) {
        let unused = self.checker.scopes.drain_unused();
        for (name, span) in unused {
            self.checker
                .warnings
                .push(TypeCheckerWarning::UnusedBinding { name, span });
        }
        self.checker.scopes.end_scope();
    }
}

impl<'a, 'src> std::ops::Deref for ScopeGuard<'a, 'src> {
    type Target = TypeChecker<'src>;
    fn deref(&self) -> &Self::Target {
        self.checker
    }
}

impl<'a, 'src> std::ops::DerefMut for ScopeGuard<'a, 'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.checker
    }
}
