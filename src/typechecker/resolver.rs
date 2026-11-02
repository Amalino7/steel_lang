use crate::parser::ast::{FnSig, NamedType, Param, TypeAst, TypeAstKind};
use crate::scanner::Span;
use crate::typechecker::core::error::{GenericError, TypeCheckerError};
use crate::typechecker::core::types::{FunctionType, NameTypeId, TupleType, Type};
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
        match &ast.kind {
            TypeAstKind::Optional(inner) => Ok(self.resolve(inner)?.wrap_in_optional()),
            TypeAstKind::Named(named) => self.resolve_named(named),
            TypeAstKind::Fn { params, ret } => {
                let params = params
                    .iter()
                    .map(|ty| Ok(("_".into(), self.resolve(ty)?)))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Type::Function(Rc::new(FunctionType {
                    is_vararg: false,
                    params,
                    return_type: self.resolve_return(ret.as_deref())?,
                    type_params: vec![],
                })))
            }
            TypeAstKind::Tuple(types) => self.resolve_tuple(types),
        }
    }

    /// An omitted annotation (`None`) is inferred: `Type::Unknown`.
    pub fn resolve_annotation(&self, ast: Option<&TypeAst<'_>>) -> Result<Type, TypeCheckerError> {
        ast.map_or(Ok(Type::Unknown), |ty| self.resolve(ty))
    }

    /// An omitted return type (`None`) is `void`.
    fn resolve_return(&self, ret: Option<&TypeAst<'_>>) -> Result<Type, TypeCheckerError> {
        ret.map_or(Ok(Type::Void), |ty| self.resolve(ty))
    }

    pub fn resolve_named(&self, named: &NamedType<'_>) -> Result<Type, TypeCheckerError> {
        let name = named.name.name;
        if name == "Self" {
            check_generic_arity(name, 0, named.args.len(), named.span)?;
            return self.self_type(named.name.span);
        }
        let resolved_generics = self.resolve_many(&named.args)?;

        self.instantiate(name, resolved_generics, named.span)
    }

    /// The `impl` target, for `Self` and `self` at `span`.
    fn self_type(&self, span: Span) -> Result<Type, TypeCheckerError> {
        self.scope_manager
            .get_self_type()
            .cloned()
            .ok_or(TypeCheckerError::SelfOutsideOfImpl { span })
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
        signature: &FnSig<'_>,
    ) -> Result<Rc<FunctionType>, TypeCheckerError> {
        let params = signature
            .params
            .items
            .iter()
            .map(|param| {
                let ty = match param {
                    Param::SelfParam(span) => self.self_type(*span)?,
                    Param::Named { ty, .. } => self.resolve(ty)?,
                };
                Ok((param.ident().name.into(), ty))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Rc::new(FunctionType {
            is_vararg: false,
            params,
            return_type: self.resolve_return(signature.ret.as_ref())?,
            type_params: self.scope_manager.active_generics(),
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
