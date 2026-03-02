use crate::parser::ast::{Binding, Expr, MatchArm, Pattern};
use crate::scanner::Span;
use crate::typechecker::core::ast::{MatchCase, StmtKind, TypedBinding, TypedExpr, TypedStmt};
use crate::typechecker::core::error::{Recoverable, TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::core::types::type_defs::EnumType;
use crate::typechecker::core::types::{GenericArgs, TupleType, Type};
use crate::typechecker::scope::guards::ScopeGuard;
use crate::typechecker::scope::manager::ScopeKind;
use crate::typechecker::scope::variables::Declaration;
use crate::typechecker::{similarity, TypeChecker};
use std::collections::HashSet;
use std::rc::Rc;

/// Accumulated state threaded through each arm of a match expression.
struct MatchContext<'src> {
    enum_def: EnumType,
    generic_args: GenericArgs,
    matched_variants: HashSet<&'src str>,
    typed_cases: Vec<MatchCase>,
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
    pub(crate) fn check_match_stmt(
        &mut self,
        value: &Expr<'src>,
        arms: &[MatchArm<'src>],
    ) -> Result<TypedStmt, TypeCheckerError> {
        let value_typed = self.check_expression(value, &Type::Unknown)?;
        let Type::Enum(enum_name, generics) = &value_typed.ty else {
            return Err(TypeCheckerError::TypeMismatch {
                expected: Type::Enum("Any".into(), vec![].into()),
                found: value_typed.ty,
                span: value_typed.span,
                message: "Match value must be an enum type.",
            });
        };

        let mut ctx = MatchContext {
            enum_def: self.sys.get_enum(enum_name).unwrap().clone(),
            generic_args: generics.clone(),
            matched_variants: HashSet::new(),
            typed_cases: vec![],
            has_fallthrough: false,
        };

        for arm in arms {
            if let Err(err) = self.handle_match_arm(&mut ctx, &value_typed, arm) {
                self.errors.push(err);
            }
        }

        check_match_exhaustiveness(&ctx, value_typed.span)?;

        Ok(TypedStmt {
            kind: StmtKind::Match {
                value: Box::new(value_typed),
                cases: ctx.typed_cases,
            },
            type_info: Type::Void,
            span: value.span(),
        })
    }

    fn handle_match_arm(
        &mut self,
        ctx: &mut MatchContext<'src>,
        value_typed: &TypedExpr,
        arm: &MatchArm<'src>,
    ) -> Result<(), TypeCheckerError> {
        if ctx.has_fallthrough {
            self.warnings.push(TypeCheckerWarning::UnreachablePattern {
                span: arm.body.span(),
                message: "Default case must be the last arm.".to_string(),
            });
            return Ok(());
        }

        match &arm.pattern {
            Pattern::Named {
                enum_name,
                variant_name,
                bind,
            } => {
                if let Some(explicit_name) = enum_name
                    && explicit_name.lexeme != ctx.enum_def.name.as_ref()
                {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Enum(ctx.enum_def.name.clone(), vec![].into()),
                        found: value_typed.ty.clone(),
                        span: value_typed.span,
                        message: "Match value must be an enum of the same type as the enum being matched against.",
                    });
                }

                let (variant_idx, payload_type): (u16, Type) = ctx
                    .enum_def
                    .get_variant_from_instance(variant_name.lexeme, &ctx.generic_args)
                    .ok_or_else(|| {
                        let variant_names: Vec<&str> =
                            ctx.enum_def.variants.keys().map(|s| s.as_ref()).collect();
                        let suggestions =
                            similarity::find_similar(variant_name.lexeme, variant_names, 3);
                        TypeCheckerError::UndefinedField {
                            struct_name: ctx.enum_def.name.to_string(),
                            field_name: variant_name.lexeme.to_string(),
                            span: variant_name.span,
                            struct_origin: Some(ctx.enum_def.origin),
                            suggestions,
                        }
                    })?;

                if ctx.matched_variants.contains(variant_name.lexeme) {
                    self.warnings.push(TypeCheckerWarning::UnreachablePattern {
                        span: arm.body.span(),
                        message: format!("Repeated pattern {}", variant_name.lexeme),
                    });
                    return Ok(());
                }
                ctx.matched_variants.insert(variant_name.lexeme);

                let (typed_binding, typed_body) = {
                    let mut guard = ScopeGuard::new(self, ScopeKind::Block);
                    let typed_binding = match bind {
                        Some(binding) => {
                            let result = guard.check_binding(binding, &payload_type, false);
                            result.recover(&mut guard.errors, TypedBinding::Ignored)
                        }
                        None => {
                            if !matches!(payload_type, Type::Void) {
                                guard.warnings.push(TypeCheckerWarning::UnusedBinding {
                                    name: format!("{} payload", variant_name.lexeme),
                                    span: variant_name.span,
                                });
                            }
                            TypedBinding::Ignored
                        }
                    };
                    let typed_body = guard.check_stmt(&arm.body);
                    (typed_binding, typed_body)
                };

                ctx.typed_cases.push(MatchCase::Named {
                    variant_idx,
                    binding: typed_binding,
                    body: typed_body,
                });
                Ok(())
            }

            Pattern::Variable(name) => {
                let (typed_binding, typed_body) = {
                    let mut guard = ScopeGuard::new(self, ScopeKind::Block);
                    let result = guard.check_binding(
                        &Binding::Variable(name.clone()),
                        &value_typed.ty,
                        false,
                    );
                    let typed_binding = result.recover(&mut guard.errors, TypedBinding::Ignored);
                    let typed_body = guard.check_stmt(&arm.body);
                    (typed_binding, typed_body)
                };
                ctx.has_fallthrough = true;
                ctx.typed_cases.push(MatchCase::Variable {
                    binding: typed_binding,
                    body: typed_body,
                });
                Ok(())
            }
        }
    }

    pub(crate) fn check_binding(
        &mut self,
        binding: &Binding,
        type_to_match: &Type,
        nested: bool,
    ) -> Result<TypedBinding, TypeCheckerError> {
        match binding {
            Binding::Struct { name, fields } => {
                let Type::Struct(struct_name, generics) = type_to_match else {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Struct("Any".into(), vec![].into()),
                        found: type_to_match.clone(),
                        span: binding.span(),
                        message: "Can only use structure destructure syntax on structs.",
                    });
                };

                // Matches plain structs ("StructName") and variant payload structs stored
                // as "EnumName.VariantName" in the type system.
                let name_matches = struct_name.as_ref() == name.lexeme
                    || struct_name
                        .rsplit_once('.')
                        .is_some_and(|(_, variant)| variant == name.lexeme);

                if !name_matches {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Struct(name.lexeme.into(), vec![].into()),
                        found: type_to_match.clone(),
                        span: binding.span(),
                        message: "Struct name in pattern does not match the expected struct type.",
                    });
                }

                let struct_def = self.sys.get_struct(struct_name.as_ref()).ok_or_else(|| {
                    TypeCheckerError::UndefinedType {
                        name: name.lexeme.to_string(),
                        span: binding.span(),
                        message: "Struct with that name wasn't found.",
                    }
                })?;

                let mut resolved: Vec<(usize, Type, &Binding)> = Vec::with_capacity(fields.len());
                for (field_name, field_binding) in fields {
                    let (field_idx, field_type) = struct_def
                        .get_field(field_name.lexeme, generics)
                        .ok_or_else(|| {
                            let field_names: Vec<&str> =
                                struct_def.fields.keys().map(|s| s.as_ref()).collect();
                            let suggestions =
                                similarity::find_similar(field_name.lexeme, field_names, 3);
                            TypeCheckerError::UndefinedField {
                                struct_name: struct_def.name.to_string(),
                                field_name: field_name.lexeme.to_string(),
                                span: field_name.span,
                                struct_origin: Some(struct_def.origin),
                                suggestions,
                            }
                        })?;
                    resolved.push((field_idx, field_type, field_binding));
                }

                let typed_fields = resolved
                    .into_iter()
                    .map(|(idx, ty, field_binding)| {
                        Ok((idx as u8, self.check_binding(field_binding, &ty, true)?))
                    })
                    .collect::<Result<Vec<_>, TypeCheckerError>>()?;

                Ok(TypedBinding::Struct(typed_fields))
            }

            Binding::Tuple { fields } => {
                let Type::Tuple(tuple) = type_to_match else {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::new_tuple(vec![Type::Any; fields.len()]),
                        found: type_to_match.clone(),
                        span: binding.span(),
                        message: "Can only use tuple destructure on tuples.",
                    });
                };

                if tuple.types.len() != fields.len() {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Tuple(Rc::new(TupleType {
                            types: vec![Type::Any; fields.len()],
                        })),
                        found: type_to_match.clone(),
                        span: binding.span(),
                        message: "Wrong number of elements in tuple destructure.",
                    });
                }

                let typed_fields = fields
                    .iter()
                    .zip(tuple.types.iter())
                    .map(|(field, ty)| self.check_binding(field, ty, true))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(TypedBinding::Tuple(typed_fields))
            }

            Binding::Variable(token) => {
                if token.lexeme == "_" {
                    return Ok(TypedBinding::Ignored);
                }
                let decl = if nested {
                    Declaration::binding(token.lexeme.into(), type_to_match.clone(), token.span)
                } else {
                    Declaration::mutable(token.lexeme.into(), type_to_match.clone(), token.span)
                };
                self.scopes.declare(decl)?;
                // TODO: migrate to a non-write lookup once one exists that still
                let (_, resolved) = self.scopes.lookup_for_write(token.lexeme).unwrap();
                Ok(TypedBinding::Variable(resolved))
            }
        }
    }
}
