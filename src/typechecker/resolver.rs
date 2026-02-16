use crate::parser::ast::{FunctionSig, TypeAst};
use crate::scanner::Token;
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::core::types::{FunctionType, TupleType, Type};
use crate::typechecker::scope::types::TypeScopeManager;
use crate::typechecker::system::TypeSystem;
use crate::typechecker::Symbol;
use std::rc::Rc;

pub struct TypeResolver<'a> {
    sys: &'a TypeSystem,
    scope_manager: &'a TypeScopeManager,
}

impl<'a> TypeResolver<'a> {
    pub fn new(sys: &'a TypeSystem, scope: &'a TypeScopeManager) -> Self {
        Self {
            sys,
            scope_manager: scope,
        }
    }

    pub fn resolve(&self, ast: &TypeAst<'_>) -> Result<Type, TypeCheckerError> {
        match ast {
            TypeAst::Optional(inner) => Ok(self.resolve(inner)?.wrap_in_optional()),
            TypeAst::Named(name, generics) => self.resolve_named(name, generics),
            TypeAst::Function(sig) => self.resolve_func(sig, vec![]).map(Type::Function),
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
        if let Some(name) = self.scope_manager.is_generic(name) {
            return Ok(Type::GenericParam(name.clone()));
        }
        let resolved_generics = self.resolve_many(generics)?;

        self.sys.instantiate(&name, resolved_generics, span)
    }

    pub fn resolve_tuple(&self, params: &[TypeAst<'_>]) -> Result<Type, TypeCheckerError> {
        let mut types = self.resolve_many(params)?;
        if types.len() == 1 {
            return Ok(types.pop().unwrap());
        }
        Ok(Type::Tuple(Rc::new(TupleType { types })))
    }

    pub fn resolve_many(&self, types: &[TypeAst<'_>]) -> Result<Vec<Type>, TypeCheckerError> {
        types.iter().map(|ty| self.resolve(ty)).collect()
    }
    pub fn resolve_func(
        &self,
        signature: &FunctionSig<'_>,
        active_generics: Vec<Symbol>,
    ) -> Result<Rc<FunctionType>, TypeCheckerError> {
        let params = signature
            .params
            .iter()
            .map(|(name, ty)| Ok((name.lexeme.into(), self.resolve(ty)?)))
            .collect::<Result<Vec<_>, _>>()?;

        let return_type = self.resolve(&signature.return_type)?;
        Ok(Rc::new(FunctionType {
            is_vararg: false,
            params,
            return_type,
            type_params: active_generics,
        }))
    }
}

pub fn convert_generics(generics: &[Token<'_>]) -> Vec<Symbol> {
    generics.iter().map(|g| g.lexeme.into()).collect()
}
