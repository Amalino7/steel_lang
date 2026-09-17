use crate::parser::ast::Literal;
use crate::scanner::Token;
use crate::typechecker::core::ast::{ExprKind, TypedExpr};
use crate::typechecker::core::error::{
    Mismatch, MismatchContext, TypeCheckerError, UndefinedMethodError,
};
use crate::typechecker::core::types::{GenericArgs, NameTypeId, Type};
use crate::typechecker::system::{TypeBlueprint, make_substitution_map};
use crate::typechecker::{TypeChecker, similarity};

impl<'src> TypeChecker<'src> {
    pub(crate) fn resolve_static_access(
        &mut self,
        type_id: NameTypeId,
        member_token: &Token,
        generics: &GenericArgs,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let enum_access = self.handle_enum_variant_access(type_id, member_token, generics);
        match enum_access {
            Some(expr) => Ok(expr),
            None => self.handle_static_method_access(type_id, member_token, generics),
        }
    }

    fn handle_enum_variant_access(
        &mut self,
        type_name: NameTypeId,
        variant_name: &Token,
        generics: &GenericArgs,
    ) -> Option<TypedExpr> {
        let NameTypeId::Enum(id) = type_name else {
            return None;
        };
        let enum_def = self.sys.get_enum(id);

        let (idx, variant_type, generics) =
            enum_def.get_static_variant(variant_name.lexeme, generics, &mut self.infer_ctx)?;

        let ty = if variant_type == Type::Void {
            Type::Enum(id, generics)
        } else {
            Type::Metatype(type_name, generics)
        };
        // Handle Type.Variant
        Some(TypedExpr {
            ty,
            kind: ExprKind::EnumInit {
                enum_name: enum_def.name.clone(),
                variant_idx: idx,
                value: Box::new(TypedExpr {
                    ty: variant_type,
                    kind: ExprKind::Literal(Literal::Nil),
                    span: variant_name.span,
                }),
            },
            span: variant_name.span,
        })
    }

    fn handle_static_method_access(
        &mut self,
        type_id: NameTypeId,
        method_name: &Token,
        generics: &GenericArgs,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let name = self.sys.get_name(type_id);
        let mangled_name = format!("{}.{}", name, method_name.lexeme);

        let methods = self.scopes.get_methods_for_type(&name);
        let method = self.scopes.lookup(mangled_name.as_str());
        let method = method.ok_or_else(|| {
            let suggestions =
                similarity::find_similar(method_name.lexeme, methods.iter().map(|s| s.as_str()), 3);
            let type_origin = self.sys.get_origin(type_id);
            let found = Type::Metatype(type_id, generics.clone());
            TypeCheckerError::UndefinedMethod(Box::new(UndefinedMethodError {
                span: method_name.span,
                found,
                method_name: method_name.lexeme.into(),
                type_origin,
                suggestions,
            }))
        })?;

        let (ctx, resolved_var) = method;
        let method_type = ctx.type_info.clone();
        let ctx_name = ctx.name.clone();

        let Type::Function(func) = &method_type else {
            unreachable!("Method should be of type function")
        };

        let impl_meta = self
            .type_scopes
            .lookup_method(&name, &method_name.lexeme)
            .map(|id| {
                let impl_info = self.sys.get_method_info(*id);
                (impl_info.self_type.clone(), impl_info.impl_generic_count)
            });

        let ty = if let Some((self_type, impl_count)) = impl_meta {
            let impl_params = &func.type_params[0..impl_count];
            let fresh_generics = self.infer_ctx.fresh_args(impl_params, &[]);
            let impl_map = make_substitution_map(impl_params, &fresh_generics);

            let method_with_fresh = method_type.generic_to_concrete(&impl_map);

            // If explicit type arguments were provided (e.g. Result.<number, number>.make)
            if !generics.is_empty() {
                let fresh_self = self_type.generic_to_concrete(&impl_map);
                let blueprint = self.sys.get_blueprint(type_id); // TODO consider instantiate?

                let concrete_type = match blueprint {
                    TypeBlueprint::Struct { id, .. } => Type::Struct(id, generics.clone()),
                    TypeBlueprint::Enum { id, .. } => Type::Enum(id, generics.clone()),
                    TypeBlueprint::Interface { id, .. } => Type::Interface(id),
                    TypeBlueprint::Primitive(inner) => inner,
                };

                self.infer_ctx
                    .unify_types(&fresh_self, &concrete_type)
                    .map_err(|unif_err| TypeCheckerError::TypeMismatch {
                        mismatch: Box::new(Mismatch::from(unif_err)),
                        context: MismatchContext::Generic,
                        primary_span: method_name.span,
                        defined_at: None,
                    })?;
            }
            self.infer_ctx.substitute(&method_with_fresh)
        } else {
            // Fallback: direct generic-name substitution.
            let params = self.sys.get_generic_param_names(type_id);
            let fresh_generics = self.infer_ctx.fresh_args(&params, generics);

            let map = make_substitution_map(&params, &fresh_generics);
            method_type.generic_to_concrete(&map)
        };

        Ok(TypedExpr {
            ty,
            kind: ExprKind::GetVar(resolved_var, ctx_name),
            span: method_name.span,
        })
    }
}
