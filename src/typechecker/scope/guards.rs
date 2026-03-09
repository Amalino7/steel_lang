use crate::scanner::{Span, Token};
use crate::typechecker::core::error::{
    DuplicateDefinition, DuplicateKind, TypeCheckerError, TypeCheckerWarning,
};
use crate::typechecker::core::types::Type;
use crate::typechecker::resolver::convert_generics;
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::scope::types::{TypeScopeError, TypeScopeKind};
use crate::typechecker::TypeChecker;

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
    pub fn new_type_params(checker: &'a mut TypeChecker<'src>, generics: &[Token<'src>]) -> Self {
        let res = checker.type_scopes.begin_type_scope(
            convert_generics(generics),
            None,
            TypeScopeKind::Type,
        );
        check_duplicate_generics(&mut checker.errors, generics, res);
        Self { checker }
    }
    pub fn new_function(checker: &'a mut TypeChecker<'src>, generics: &[Token<'src>]) -> Self {
        let res = checker.type_scopes.begin_type_scope(
            convert_generics(generics),
            None,
            TypeScopeKind::Function,
        );
        check_duplicate_generics(&mut checker.errors, generics, res);
        TypeScopeGuard { checker }
    }
    pub fn new_impl(checker: &'a mut TypeChecker<'src>, self_ty: Type) -> Self {
        let _ = checker
            .type_scopes
            .begin_type_scope(vec![], Some(self_ty), TypeScopeKind::Impl);
        TypeScopeGuard { checker }
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
