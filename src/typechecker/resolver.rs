use crate::parser::ast::{FunctionSig, TypeAst};
use crate::scanner::{Span, Token};
use crate::typechecker::core::error::{GenericError, TypeCheckerError};
use crate::typechecker::core::types::{FunctionType, GenericTypeId, NameTypeId, TupleType, Type};
use crate::typechecker::scope::types::TypeScopeManager;
use crate::typechecker::system::{TypeBlueprint, TypeSystem};
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
            check_generic_arity(name, 0, generics.len(), span)?;
            return self
                .scope_manager
                .get_self_type()
                .cloned()
                .ok_or(TypeCheckerError::SelfOutsideOfImpl { span: token.span });
        }
        let resolved_generics = self.resolve_many(generics)?;

        self.instantiate(name, resolved_generics, span)
    }

    pub fn instantiate(
        &self,
        name: &str,
        generics: Vec<Type>,
        source: Span,
    ) -> Result<Type, TypeCheckerError> {
        let id = self.scope_manager.lookup_type(name).ok_or_else(|| {
            TypeCheckerError::UndefinedType {
                name: name.to_string(),
                span: source,
                message: "Could not find type with that name.",
            }
        })?;

        let blueprint = self.sys.get_blueprint(id);

        match blueprint {
            TypeBlueprint::Primitive(ty) => {
                check_generic_arity(name, 0, generics.len(), source)?;
                Ok(ty)
            }
            TypeBlueprint::Struct { id, arity } => {
                check_generic_arity(name, arity, generics.len(), source)?;
                Ok(Type::Struct(id, Rc::from(generics)))
            }
            TypeBlueprint::Enum { id, arity } => {
                check_generic_arity(name, arity, generics.len(), source)?;
                Ok(Type::Enum(id, Rc::from(generics)))
            }
            TypeBlueprint::Interface { id } => {
                check_generic_arity(name, 0, generics.len(), source)?;
                Ok(Type::Interface(id))
            }
            TypeBlueprint::Generic { id } => {
                check_generic_arity(name, 0, generics.len(), source)?;
                Ok(Type::GenericParam(id))
            }
        }
    }

    pub fn get_type_id(&self, name: &str) -> Option<NameTypeId> {
        self.scope_manager.lookup_type(name)
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
    pub fn resolve_generic_func(
        &self,
        signature: &FunctionSig<'_>,
    ) -> Result<Rc<FunctionType>, TypeCheckerError> {
        self.resolve_func(signature, self.scope_manager.active_generics())
    }

    fn resolve_func(
        &self,
        signature: &FunctionSig<'_>,
        active_generics: Vec<GenericTypeId>,
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

fn check_generic_arity(
    type_name: &str,
    generics_expected: usize,
    generics_provided: usize,
    span: Span,
) -> Result<(), TypeCheckerError> {
    if generics_provided != generics_expected {
        Err(TypeCheckerError::Generic(GenericError::CountMismatch {
            span,
            found: generics_provided,
            expected: generics_expected,
            type_name: type_name.to_string(),
        }))
    } else {
        Ok(())
    }
}
