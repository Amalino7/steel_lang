use crate::parser::ast::Literal;
use crate::scanner::Token;
use crate::typechecker::core::ast::{ExprKind, TypedExpr};
use crate::typechecker::core::error::{Mismatch, MismatchContext, TypeCheckerError};
use crate::typechecker::core::types::{GenericArgs, Type};
use crate::typechecker::system::{make_substitution_map, TypeBlueprint};
use crate::typechecker::{similarity, Symbol, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub(crate) fn resolve_static_access(
        &mut self,
        type_token: &Symbol,
        member_token: &Token,
        generics: &GenericArgs,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let enum_access = self.handle_enum_variant_access(type_token, member_token, generics);
        match enum_access {
            Some(expr) => Ok(expr),
            None => self.handle_static_method_access(type_token, member_token, generics),
        }
    }

    fn handle_enum_variant_access(
        &mut self,
        type_name: &Symbol,
        variant_name: &Token,
        generics: &GenericArgs,
    ) -> Option<TypedExpr> {
        let enum_def = self.sys.get_enum(type_name)?;

        let (idx, variant_type, generics) =
            enum_def.get_static_variant(variant_name.lexeme, generics, &mut self.infer_ctx)?;

        let ty = if variant_type == Type::Void {
            Type::Enum(enum_def.name.clone(), generics)
        } else {
            Type::Metatype(type_name.clone(), generics)
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
        type_name: &Symbol,
        method_name: &Token,
        generics: &GenericArgs,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mangled_name = format!("{}.{}", type_name, method_name.lexeme);

        let methods = self.scopes.get_methods_for_type(type_name);
        let method = self.scopes.lookup(mangled_name.as_str());
        let method = method.ok_or_else(|| {
            let suggestions =
                similarity::find_similar(method_name.lexeme, methods.iter().map(|s| s.as_str()), 3);
            let type_origin = self.sys.get_origin(type_name);
            let found = Type::Metatype(type_name.clone(), generics.clone());
            TypeCheckerError::UndefinedMethod {
                span: method_name.span,
                found,
                method_name: method_name.lexeme.to_string(),
                type_origin,
                suggestions,
            }
        })?;

        let (ctx, resolved_var) = method;
        let method_type = ctx.type_info.clone();
        let ctx_name = ctx.name.clone();

        let Type::Function(func) = &method_type else {
            unreachable!("Method should be of type function")
        };

        let impl_meta: Option<(usize, Type)> = self
            .sys
            .get_method_info(&mangled_name)
            .map(|info| (info.impl_generic_count, info.self_type.clone()));

        let ty = if let Some((impl_count, self_type)) = impl_meta {
            let impl_params = &func.type_params[0..impl_count];
            let fresh_generics = self.infer_ctx.fresh_args(impl_params, &[]);
            let impl_map = make_substitution_map(impl_params, &fresh_generics);

            let method_with_fresh = method_type.generic_to_concrete(&impl_map);

            // If explicit type arguments were provided (e.g. Result.<number, number>.make)
            if !generics.is_empty() {
                let fresh_self = self_type.generic_to_concrete(&impl_map);
                let blueprint = self
                    .sys
                    .get_blueprint(type_name)
                    .expect("Type should exist");

                let concrete_type = match blueprint {
                    TypeBlueprint::Struct { .. } => {
                        Type::Struct(type_name.clone(), generics.clone())
                    }
                    TypeBlueprint::Enum { .. } => Type::Enum(type_name.clone(), generics.clone()),
                    TypeBlueprint::Interface { .. } => Type::Interface(type_name.clone()),
                    TypeBlueprint::Primitive(inner) => inner,
                };

                self.infer_ctx
                    .unify_types(&fresh_self, &concrete_type)
                    .map_err(|unif_err| TypeCheckerError::TypeMismatch {
                        mismatch: Mismatch::from(unif_err),
                        context: MismatchContext::Generic,
                        primary_span: method_name.span,
                        defined_at: None,
                    })?;
            }
            let res = self.infer_ctx.substitute(&method_with_fresh);
            res
        } else {
            // Fallback: direct generic-name substitution.
            let params = self.sys.get_generic_param_names(type_name);
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
