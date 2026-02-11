use crate::parser::ast::{FunctionSig, TypeAst};
use crate::scanner::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::{TupleType, Type};
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashSet;
use std::rc::Rc;

pub struct TypeScopeGuard<'a, 'src> {
    type_checker: &'a mut TypeChecker<'src>,
    generics: Vec<Symbol>,
    has_self_type: bool,
}
/// Duplicate generic definition.
type TypeResolverError = Symbol;

impl<'a, 'src> TypeScopeGuard<'a, 'src> {
    pub fn new(
        checker: &'a mut TypeChecker<'src>,
        generics: &[Token],
    ) -> Result<Self, TypeResolverError> {
        let mut symbol_generic = vec![];
        for generic in generics.iter() {
            if checker.resolver.generics.contains(generic.lexeme) {
                return Err(generic.lexeme.into());
            }
            symbol_generic.push(generic.lexeme.into())
        }
        checker.resolver.generics.extend(symbol_generic.clone());
        Ok(TypeScopeGuard {
            type_checker: checker,
            generics: symbol_generic,
            has_self_type: false,
        })
    }
    pub fn new_impl_scope(
        checker: &'a mut TypeChecker<'src>,
        generics: &[Token],
        self_type: Type,
    ) -> Result<Self, TypeResolverError> {
        let mut symbol_generic = vec![];
        for generic in generics.iter() {
            if checker.resolver.generics.contains(generic.lexeme) {
                return Err(generic.lexeme.into());
            }
            symbol_generic.push(generic.lexeme.into())
        }
        checker.resolver.generics.extend(symbol_generic.clone());
        checker.resolver.self_type = Some(self_type);
        Ok(TypeScopeGuard {
            type_checker: checker,
            generics: symbol_generic,
            has_self_type: true,
        })
    }
}
impl<'a> Drop for TypeScopeGuard<'a, '_> {
    fn drop(&mut self) {
        for generic in self.generics.iter() {
            self.type_checker.resolver.generics.remove(generic);
        }
        if self.has_self_type {
            self.type_checker.resolver.self_type = None;
        }
    }
}

impl<'a, 'src> std::ops::Deref for TypeScopeGuard<'a, 'src> {
    type Target = TypeChecker<'src>;
    fn deref(&self) -> &Self::Target {
        &self.type_checker
    }
}

impl<'a, 'src> std::ops::DerefMut for TypeScopeGuard<'a, 'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.type_checker
    }
}
pub struct TypeResolver {
    pub sys: TypeSystem,
    generics: HashSet<Symbol>,
    self_type: Option<Type>,
}

impl TypeResolver {
    pub fn new(sys: TypeSystem) -> Self {
        Self {
            sys,
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

    pub fn resolve(&self, ast: &TypeAst<'_>) -> Result<Type, TypeCheckerError> {
        match ast {
            TypeAst::Optional(inner) => Ok(self.resolve(inner)?.wrap_in_optional()),
            TypeAst::Named(name, generics) => self.resolve_named(name, generics),
            TypeAst::Function(sig) => self.resolve_func(sig, &[]),
            TypeAst::Tuple(param) => self.resolve_tuple(param),
            TypeAst::Infer => Ok(Type::Unknown),
        }
    }
    pub fn resolve_named(
        &self,
        token: &Token,
        generics: &[TypeAst<'_>],
    ) -> Result<Type, TypeCheckerError> {
        let span = token.span.merge(
            generics
                .last()
                .map(|t: &TypeAst| t.span())
                .unwrap_or(token.span),
        );

        let name = token.lexeme;
        if name == "Self" {
            return self
                .self_type
                .clone()
                .ok_or(TypeCheckerError::SelfOutsideOfImpl { span: token.span });
        }
        if let Some(name) = self.generics.get(name) {
            return Ok(Type::GenericParam(name.clone()));
        }
        let resolved_generics = self.resolve_many(generics)?;

        self.sys.instantiate(&name, resolved_generics, span)
    }

    pub fn resolve_tuple(&self, params: &[TypeAst<'_>]) -> Result<Type, TypeCheckerError> {
        let types = self.resolve_many(params)?;
        Ok(Type::Tuple(Rc::new(TupleType { types })))
    }

    pub fn resolve_many(&self, types: &[TypeAst<'_>]) -> Result<Vec<Type>, TypeCheckerError> {
        types.iter().map(|ty| self.resolve(ty)).collect()
    }
    pub fn resolve_func(
        &self,
        signature: &FunctionSig<'_>,
        generics: &[Token],
    ) -> Result<Type, TypeCheckerError> {
        let params = signature
            .params
            .iter()
            .map(|(name, ty)| Ok((name.lexeme.to_string(), self.resolve(ty)?)))
            .collect::<Result<Vec<_>, _>>()?;

        let generics = generics.iter().map(|s| s.lexeme.into()).collect();

        let return_type = self.resolve(&signature.return_type)?;
        Ok(Type::new_function(params, return_type, generics))
    }
}
