use crate::parser::ast::{Binding, BindingKind, Expr, FieldBinding, Ident, MatchArm, PatternKind};
use crate::scanner::Span;
use crate::typechecker::core::ast::{ExprKind, MatchCase, TypedBinding, TypedExpr};
use crate::typechecker::core::error::TypeRequirement::Structural;
use crate::typechecker::core::error::{
    Mismatch, MismatchContext, Operand, Recoverable, TypeCheckerError, TypeCheckerWarning,
};
use crate::typechecker::core::types::type_defs::EnumType;
use crate::typechecker::core::types::{GenericArgs, StructId, TupleType, Type};
use crate::typechecker::scope::guards::ScopeGuard;
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::scope::variables::Declaration;
use crate::typechecker::{TypeChecker, similarity};
use std::collections::HashSet;
use std::rc::Rc;

/// Accumulated state threaded through each arm of a match expression.
struct MatchContext<'src> {
    enum_def: EnumType,
    generic_args: GenericArgs,
    matched_variants: HashSet<&'src str>,
    has_fallthrough: bool,
}

fn check_match_exhaustiveness(ctx: &MatchContext<'_>, span: Span) -> Result<(), TypeCheckerError> {
    if ctx.has_fallthrough {
        return Ok(());
    }
    for variant in ctx.enum_def.variants.keys() {
        if !ctx.matched_variants.contains(variant.as_ref()) {
            return Err(TypeCheckerError::UncoveredPattern {
                variant: variant.to_string(),
                span,
            });
        }
    }
    Ok(())
}

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_match_expr(
        &mut self,
        scrutinee: &Expr<'src>,
        arms: &[MatchArm<'src>],
        span: Span,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let value_typed = self.check_expression(scrutinee, &Type::Unknown);
        let Type::Enum(enum_id, generics) = &value_typed.ty else {
            return Err(TypeCheckerError::OperatorConstraint {
                operator: "match",
                operand: Operand::Unary,
                found: value_typed.ty,
                requirement: Structural("Enum"),
                span: value_typed.span,
            });
        };

        let mut ctx = MatchContext {
            enum_def: self.sys.get_enum(*enum_id).clone(),
            generic_args: generics.clone(),
            matched_variants: HashSet::new(),
            has_fallthrough: false,
        };

        let mut expr_cases: Vec<MatchCase> = vec![];
        let mut unified_type: Option<Type> = None;

        for arm in arms {
            if ctx.has_fallthrough {
                self.warn(TypeCheckerWarning::UnreachablePattern {
                    span: arm.span,
                    message: "Default case must be the last arm.".to_string(),
                });
                continue;
            }

            let arm_result =
                self.handle_match_arm(&mut ctx, &value_typed, arm, unified_type.as_ref());

            match arm_result {
                Ok((case, arm_ty)) => {
                    unified_type = unified_type.or(Some(arm_ty));
                    expr_cases.push(case);
                }
                Err(e) => self.report(e),
            }
        }

        check_match_exhaustiveness(&ctx, value_typed.span)?;

        let result_type = unified_type.unwrap_or(Type::Void);
        Ok(TypedExpr {
            ty: result_type,
            kind: ExprKind::Match {
                value: Box::new(value_typed),
                cases: expr_cases,
            },
            span,
        })
    }

    fn handle_match_arm(
        &mut self,
        ctx: &mut MatchContext<'src>,
        value_typed: &TypedExpr,
        arm: &MatchArm<'src>,
        expected_ty: Option<&Type>,
    ) -> Result<(MatchCase, Type), TypeCheckerError> {
        let hint = expected_ty.unwrap_or(&Type::Unknown);

        match &arm.pattern.kind {
            PatternKind::Variant {
                enum_name,
                variant: variant_name,
                payload,
            } => {
                if let Some(explicit_name) = enum_name
                    && explicit_name.name != ctx.enum_def.name.as_ref()
                {
                    return Err(TypeCheckerError::TypeMismatch {
                        mismatch: Mismatch::simple(
                            Type::Enum(ctx.enum_def.id, vec![].into()),
                            value_typed.ty.clone(),
                        ),
                        context: MismatchContext::Generic,
                        primary_span: value_typed.span,
                        defined_at: None,
                    });
                }

                let (variant_idx, payload_type) = ctx
                    .enum_def
                    .get_variant_from_instance(variant_name.name, &ctx.generic_args)
                    .ok_or_else(|| {
                        let variant_names: Vec<&str> =
                            ctx.enum_def.variants.keys().map(|s| s.as_ref()).collect();
                        let suggestions =
                            similarity::find_similar(variant_name.name, variant_names, 3);
                        TypeCheckerError::UndefinedField {
                            struct_name: ctx.enum_def.name.clone(),
                            field_name: variant_name.name.into(),
                            span: variant_name.span,
                            struct_origin: Some(ctx.enum_def.origin),
                            suggestions,
                        }
                    })?;

                if ctx.matched_variants.contains(variant_name.name) {
                    self.warn(TypeCheckerWarning::UnreachablePattern {
                        span: arm.span,
                        message: format!("Repeated pattern {}", variant_name.name),
                    });
                }
                ctx.matched_variants.insert(variant_name.name);

                let (typed_binding, typed_body) = {
                    let mut guard = ScopeGuard::new(self, ScopeKind::Block);
                    let typed_binding = match payload {
                        Some(binding) => {
                            let result = guard.check_binding(binding, &payload_type, false, false);
                            result.recover(&mut guard.errors, TypedBinding::Ignored)
                        }
                        None => {
                            if !matches!(payload_type, Type::Void) {
                                guard.warn(TypeCheckerWarning::UnusedBinding {
                                    name: format!("{} payload", variant_name.name),
                                    span: variant_name.span,
                                });
                            }
                            TypedBinding::Ignored
                        }
                    };
                    let typed_body =
                        guard.coerce_expression(&arm.body, hint, MismatchContext::Generic, None);
                    (typed_binding, typed_body)
                };

                let arm_ty = typed_body.ty.clone();
                Ok((
                    MatchCase::Named {
                        variant_idx,
                        binding: typed_binding,
                        body: typed_body,
                    },
                    arm_ty,
                ))
            }

            PatternKind::Name(_) | PatternKind::Wildcard => {
                let binding = Binding {
                    kind: match &arm.pattern.kind {
                        PatternKind::Name(name) => BindingKind::Name(*name),
                        _ => BindingKind::Wildcard,
                    },
                    span: arm.pattern.span,
                };
                let (typed_binding, typed_body) = {
                    let mut guard = ScopeGuard::new(self, ScopeKind::Block);
                    let result = guard.check_binding(&binding, &value_typed.ty, false, false);
                    let typed_binding = result.recover(&mut guard.errors, TypedBinding::Ignored);
                    let typed_body =
                        guard.coerce_expression(&arm.body, hint, MismatchContext::Generic, None);
                    (typed_binding, typed_body)
                };
                ctx.has_fallthrough = true;
                let arm_ty = typed_body.ty.clone();
                Ok((
                    MatchCase::Variable {
                        binding: typed_binding,
                        body: typed_body,
                    },
                    arm_ty,
                ))
            }
        }
    }

    pub(crate) fn check_binding(
        &mut self,
        binding: &Binding,
        type_to_match: &Type,
        nested: bool,
        is_public: bool,
    ) -> Result<TypedBinding, TypeCheckerError> {
        match &binding.kind {
            BindingKind::Struct { name, fields } => {
                let Type::Struct(struct_id, generics) = type_to_match else {
                    return Err(TypeCheckerError::TypeMismatch {
                        mismatch: Mismatch::simple(
                            Type::Struct(StructId(0), vec![].into()), // TODO error report
                            type_to_match.clone(),
                        ),
                        context: MismatchContext::Generic,
                        primary_span: binding.span,
                        defined_at: None,
                    });
                };

                // Matches plain structs ("StructName") and variant payload structs stored
                // as "EnumName.VariantName" in the type system.
                let struct_name = self.sys.get_struct(*struct_id).name.as_ref();
                let name_matches = struct_name == name.name
                    || struct_name
                        .rsplit_once('.')
                        .is_some_and(|(_, variant)| variant == name.name);

                if !name_matches {
                    return Err(TypeCheckerError::TypeMismatch {
                        mismatch: Mismatch::simple(
                            Type::Struct(*struct_id, vec![].into()),
                            type_to_match.clone(),
                        ),
                        context: MismatchContext::Generic,
                        primary_span: binding.span,
                        defined_at: None,
                    });
                }

                let struct_def = self.sys.get_struct(*struct_id);

                let mut resolved: Vec<(usize, Type, &Binding, &Ident)> =
                    Vec::with_capacity(fields.len());
                for FieldBinding {
                    field: field_name,
                    binding: field_binding,
                    ..
                } in fields
                {
                    let (field_idx, field_type) = struct_def
                        .get_field(field_name.name, generics)
                        .ok_or_else(|| {
                        let field_names: Vec<&str> = struct_def
                            .visible_field_names(self.file_id)
                            .map(|s| s.as_ref())
                            .collect();
                        let suggestions = similarity::find_similar(field_name.name, field_names, 3);
                        TypeCheckerError::UndefinedField {
                            struct_name: struct_def.name.clone(),
                            field_name: field_name.name.into(),
                            span: field_name.span,
                            struct_origin: Some(struct_def.origin),
                            suggestions,
                        }
                    })?;
                    resolved.push((field_idx, field_type, field_binding, field_name));
                }

                for (idx, _, _, field_name) in &resolved {
                    self.check_field_access(type_to_match, *idx as u8, field_name);
                }

                let typed_fields = resolved
                    .into_iter()
                    .map(|(idx, ty, field_binding, _)| {
                        Ok((
                            idx as u8,
                            self.check_binding(field_binding, &ty, true, is_public)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, TypeCheckerError>>()?;

                Ok(TypedBinding::Struct(typed_fields))
            }

            BindingKind::Tuple(fields) => {
                let Type::Tuple(tuple) = type_to_match else {
                    return Err(TypeCheckerError::TypeMismatch {
                        mismatch: Mismatch::simple(
                            Type::new_tuple(vec![Type::Any; fields.len()]),
                            type_to_match.clone(),
                        ),
                        context: MismatchContext::Generic,
                        primary_span: binding.span,
                        defined_at: None,
                    });
                };

                if tuple.types.len() != fields.len() {
                    return Err(TypeCheckerError::TypeMismatch {
                        mismatch: Mismatch::simple(
                            Type::Tuple(Rc::new(TupleType {
                                types: vec![Type::Any; fields.len()],
                            })),
                            type_to_match.clone(),
                        ),
                        context: MismatchContext::Generic,
                        primary_span: binding.span,
                        defined_at: None,
                    });
                }

                let typed_fields = fields
                    .iter()
                    .zip(tuple.types.iter())
                    .map(|(field, ty)| self.check_binding(field, ty, true, is_public))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(TypedBinding::Tuple(typed_fields))
            }

            BindingKind::Wildcard => Ok(TypedBinding::Ignored),
            BindingKind::Name(token) => {
                let decl = if nested {
                    Declaration::binding(token.name.into(), type_to_match.clone(), token.span)
                } else {
                    Declaration::mutable(token.name.into(), type_to_match.clone(), token.span)
                }
                .public(is_public);
                self.scopes.declare(decl)?;
                // TODO: migrate to a non-write lookup once one exists that still
                let (ctx, resolved) = self.scopes.lookup_for_write(token.name).unwrap();
                Ok(TypedBinding::Variable(resolved, ctx.name.clone()))
            }
        }
    }
}
