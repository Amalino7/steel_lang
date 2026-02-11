use crate::parser::ast::TypeAst;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_registry::TypeRegistry;
use crate::typechecker::types::Type;
use crate::typechecker::Symbol;
use std::collections::HashSet;

pub struct TypeScopeGuard<'a> {
    resolver: &'a mut TypeResolver<'a>,
    generics: Vec<Symbol>,
    has_self_type: bool,
}
/// Duplicate generic definition.
type TypeResolverError = Symbol;

impl<'a> TypeScopeGuard<'a> {
    pub fn new(
        resolver: &'a mut TypeResolver<'a>,
        generics: Vec<Symbol>,
    ) -> Result<Self, TypeResolverError> {
        for generic in generics.iter() {
            if resolver.generics.contains(generic) {
                return Err(generic.clone());
            }
        }
        resolver.generics.extend(generics.iter().cloned());
        Ok(TypeScopeGuard {
            resolver,
            generics,
            has_self_type: false,
        })
    }
    pub fn new_impl_scope(
        resolver: &'a mut TypeResolver<'a>,
        generics: Vec<Symbol>,
        self_type: Type,
    ) -> Result<Self, TypeResolverError> {
        for generic in generics.iter() {
            if resolver.generics.contains(generic) {
                return Err(generic.clone());
            }
        }
        resolver.generics.extend(generics.iter().cloned());
        resolver.self_type = Some(self_type);
        Ok(TypeScopeGuard {
            resolver,
            generics,
            has_self_type: true,
        })
    }
}
impl<'a> Drop for TypeScopeGuard<'a> {
    fn drop(&mut self) {
        for generic in self.generics.iter() {
            self.resolver.generics.remove(generic);
        }
        if self.has_self_type {
            self.resolver.self_type = None;
        }
    }
}

impl<'a> std::ops::Deref for TypeScopeGuard<'a> {
    type Target = TypeResolver<'a>;
    fn deref(&self) -> &Self::Target {
        self.resolver
    }
}

impl<'a> std::ops::DerefMut for TypeScopeGuard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.resolver
    }
}
pub struct TypeResolver<'reg> {
    registry: &'reg TypeRegistry,
    generics: HashSet<Symbol>,
    self_type: Option<Type>,
}

impl<'reg> TypeResolver<'reg> {
    pub fn new(registry: &'reg TypeRegistry) -> Self {
        Self {
            registry,
            generics: HashSet::new(),
            self_type: None,
        }
    }
    pub fn get_generics(&self) -> &HashSet<Symbol> {
        &self.generics
    }
    pub fn get_self_type(&self) -> Option<&Type> {
        self.self_type.as_ref()
    }

    pub fn resolve(&self, ast: &TypeAst) -> Result<Type, TypeCheckerError> {
        match ast {
            TypeAst::Optional(_) => {}
            TypeAst::Named(name, generics) => self.resolve_named(name, generics)?,
            TypeAst::Function { .. } => {}
            TypeAst::Tuple(_) => {}
            TypeAst::Infer => {}
        }
        todo!()
    }
}
