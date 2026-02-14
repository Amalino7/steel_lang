use crate::typechecker::scope::scope_manager::ScopeKind;
use crate::typechecker::type_resolver::TypeResolver;
use crate::typechecker::types::Type;
use crate::typechecker::{Symbol, TypeChecker};

pub struct ScopeGuard<'a, 'src> {
    checker: &'a mut TypeChecker<'src>,
    has_var_scope: bool,
    has_type_scope: bool,
}

pub struct PartialGuard<'a, 'src> {
    checker: Option<&'a mut TypeChecker<'src>>,
}

impl<'a, 'src> Drop for PartialGuard<'a, 'src> {
    fn drop(&mut self) {
        if let Some(tc) = self.checker.as_mut() {
            tc.scopes.end_type_scope()
        }
    }
}

impl<'a, 'src> PartialGuard<'a, 'src> {
    pub fn new_generics(checker: &'a mut TypeChecker<'src>, generics: &[Symbol]) -> Self {
        checker.scopes.begin_type_scope(generics, None);
        PartialGuard {
            checker: Some(checker),
        }
    }
    pub fn res(&self) -> TypeResolver {
        self.checker.as_ref().expect("Invalid guard usage").res()
    }
    pub fn all_generics(&self) -> Vec<Symbol> {
        self.checker
            .as_ref()
            .expect("Invalid guard usage")
            .scopes
            .all_generics()
    }

    #[must_use]
    pub fn upgrade_impl(mut self, self_ty: Type) -> ScopeGuard<'a, 'src> {
        self.checker
            .as_mut()
            .expect("Invalid upgrade usage!")
            .scopes
            .set_self(self_ty);
        ScopeGuard {
            checker: self.checker.take().expect("Invalid upgrade usage!"),
            has_var_scope: false,
            has_type_scope: true,
        }
    }

    #[must_use]
    pub fn upgrade_function(mut self) -> ScopeGuard<'a, 'src> {
        self.checker
            .as_mut()
            .expect("Invalid upgrade usage!")
            .scopes
            .begin_scope(ScopeKind::Function);
        ScopeGuard {
            checker: self.checker.take().expect("Invalid upgrade usage!"),
            has_var_scope: true,
            has_type_scope: true,
        }
    }
}

impl<'a, 'src> ScopeGuard<'a, 'src> {
    pub fn new(checker: &'a mut TypeChecker<'src>, kind: ScopeKind) -> Self {
        checker.scopes.begin_scope(kind);
        ScopeGuard {
            checker,
            has_var_scope: true,
            has_type_scope: false,
        }
    }

    pub fn new_type_params(checker: &'a mut TypeChecker<'src>, generics: &[Symbol]) -> Self {
        checker.scopes.begin_type_scope(generics, None);
        ScopeGuard {
            checker,
            has_var_scope: false,
            has_type_scope: true,
        }
    }
}
impl<'a, 'src> Drop for ScopeGuard<'a, 'src> {
    fn drop(&mut self) {
        if self.has_var_scope {
            self.checker.scopes.end_scope();
        }
        if self.has_type_scope {
            self.checker.scopes.end_type_scope();
        }
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
