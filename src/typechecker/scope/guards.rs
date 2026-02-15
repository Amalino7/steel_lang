use crate::typechecker::core::types::Type;
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::scope::types::TypeScopeKind;
use crate::typechecker::{Symbol, TypeChecker};

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
    pub fn new_type_params(checker: &'a mut TypeChecker<'src>, generics: &[Symbol]) -> Self {
        checker
            .type_scopes
            .begin_type_scope(generics, None, TypeScopeKind::Type);
        Self { checker }
    }
    pub fn new_function(checker: &'a mut TypeChecker<'src>, generics: &[Symbol]) -> Self {
        checker
            .type_scopes
            .begin_type_scope(generics, None, TypeScopeKind::Function);
        TypeScopeGuard { checker }
    }
    pub fn new_impl(checker: &'a mut TypeChecker<'src>, self_ty: Type) -> Self {
        checker
            .type_scopes
            .begin_type_scope(&[], Some(self_ty), TypeScopeKind::Impl);
        TypeScopeGuard { checker }
    }
}

impl<'a, 'src> ScopeGuard<'a, 'src> {
    pub fn new(checker: &'a mut TypeChecker<'src>, kind: ScopeKind) -> Self {
        checker.scopes.begin_scope(kind);
        ScopeGuard { checker }
    }
}
impl<'a, 'src> Drop for ScopeGuard<'a, 'src> {
    fn drop(&mut self) {
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
