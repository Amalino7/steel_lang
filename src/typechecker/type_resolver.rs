use crate::parser::ast::{FunctionSig, TypeAst};
use crate::scanner::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::scope_manager::ScopeManager;
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::{TupleType, Type};
use crate::typechecker::{Symbol, TypeChecker};
use std::rc::Rc;

pub struct TypeScopeGuard<'a, 'src> {
    type_checker: &'a mut TypeChecker<'src>,
    generics: Vec<Symbol>,
    has_self_type: bool,
}
pub struct TypeResolver<'a> {
    sys: &'a TypeSystem,
    scope_manager: &'a ScopeManager,
}

impl<'a> TypeResolver<'a> {
    pub fn new(sys: &'a TypeSystem, scope: &'a ScopeManager) -> Self {
        Self {
            sys,
            scope_manager: scope,
        }
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
                .scope_manager
                .get_self_type()
                .cloned()
                .ok_or(TypeCheckerError::SelfOutsideOfImpl { span: token.span });
        }
        if let Some(name) = self.scope_manager.get_generics().get(name) {
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
