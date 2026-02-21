use crate::parser::ast::Literal;
use crate::scanner::Token;
use crate::typechecker::core::ast::{ExprKind, TypedExpr};
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::core::types::{GenericArgs, Type};
use crate::typechecker::system::generics_to_map;
use crate::typechecker::{Symbol, TypeChecker};
use std::rc::Rc;

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
            enum_def.get_variant(variant_name.lexeme, generics, &mut self.infer_ctx)?;

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
            let suggestions = crate::typechecker::similarity::find_similar(
                method_name.lexeme,
                methods.iter().map(|s| s.as_str()),
                3,
            );
            TypeCheckerError::UndefinedMethod {
                span: method_name.span,
                found: Type::Struct(Rc::from(type_name.to_string()), vec![].into()),
                method_name: method_name.lexeme.to_string(),
                type_origin: None, // TODO: track struct definition span
                suggestions,
            }
        })?;

        let (ctx, resolved_var) = method;
        let params = self.sys.get_generic_param_names(type_name);
        let pairs = generics_to_map(&params, generics, Some(&mut self.infer_ctx));
        let ty = ctx.type_info.clone().generic_to_concrete(&pairs);
        Ok(TypedExpr {
            ty,
            kind: ExprKind::GetVar(resolved_var, ctx.name.clone()),
            span: method_name.span,
        })
    }
}
