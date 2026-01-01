use crate::parser::ast::Literal;
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{ExprKind, TypedExpr};
use crate::typechecker::type_system::TypeSystem;
use crate::typechecker::types::{generics_to_map, GenericArgs, Type};
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashMap;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn resolve_static_access(
        &mut self,
        type_token: &Symbol,
        member_token: &Token,
        generics: &GenericArgs,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let enum_access = self.handle_enum_access(type_token, member_token, generics);
        match enum_access {
            Some(expr) => Ok(expr),
            None => self.handle_static_method_access(type_token, member_token, generics),
        }
    }

    fn handle_enum_access(
        &mut self,
        type_name: &Symbol,
        variant_name: &Token,
        generics: &GenericArgs,
    ) -> Option<TypedExpr> {
        let enum_def = self.sys.get_enum(type_name)?;

        let (idx, variant_types) = enum_def.get_variant(variant_name.lexeme)?;

        let ty = if variant_types == Type::Void {
            Type::Enum(enum_def.name.clone(), vec![].into())
        } else {
            Type::Metatype(type_name.clone(), generics.clone())
        };
        // Handle Type.Variant
        Some(TypedExpr {
            ty,
            kind: ExprKind::EnumInit {
                enum_name: enum_def.name.clone(),
                variant_idx: idx as u16,
                value: Box::new(TypedExpr {
                    ty: Type::Nil,
                    kind: ExprKind::Literal(Literal::Nil),
                    line: variant_name.line,
                }),
            },
            line: variant_name.line,
        })
    }

    pub(crate) fn handle_enum_call(
        &self,
        variant_name: &str,
        variant_type: &Type,
        type_params: &[Symbol],
        generic_args: &GenericArgs,
        inferred_args: Vec<(Option<&str>, TypedExpr, u32)>,
        line: u32,
    ) -> Result<(TypedExpr, HashMap<Symbol, Type>), TypeCheckerError> {
        let mut map = generics_to_map(type_params, generic_args);

        let bind_args = |args: &[(String, Type)]| {
            self.sys
                .bind_arguments(&variant_name, &mut map, args, inferred_args, false, line)
        };

        let val_expr = match variant_type {
            Type::Struct(struct_name, empty) => {
                let struct_def = self
                    .sys
                    .get_struct(struct_name)
                    .expect("Enum variant points to non-existent struct");

                let bound_args = bind_args(&struct_def.ordered_fields)?;

                TypedExpr {
                    ty: Type::Struct(struct_name.clone(), empty.clone()),
                    kind: ExprKind::StructInit {
                        name: Box::from(struct_name.to_string()),
                        args: bound_args,
                    },
                    line,
                }
            }
            // Tuple variant
            Type::Tuple(tuple_types) => {
                let params: Vec<(String, Type)> = tuple_types
                    .types
                    .iter()
                    .enumerate()
                    .map(|(i, t)| (i.to_string(), t.clone()))
                    .collect();

                let bound_args = bind_args(&params)?;

                TypedExpr {
                    ty: variant_type.clone(),
                    kind: ExprKind::Tuple {
                        elements: bound_args,
                    },
                    line,
                }
            }
            _ => {
                // One argument
                let params = vec![("value".to_string(), variant_type.clone())];
                let mut bound_args = bind_args(&params)?;
                bound_args.pop().unwrap() // Safe because bind_arguments guarantees match
            }
        };
        Ok((val_expr, map))
    }

    fn handle_static_method_access(
        &mut self,
        type_name: &Symbol,
        method_name: &Token,
        generics: &GenericArgs,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mangled_name = format!("{}.{}", type_name, method_name.lexeme);

        let method =
            self.scopes
                .lookup(mangled_name.as_str())
                .ok_or(TypeCheckerError::UndefinedMethod {
                    line: method_name.line,
                    found: Type::Struct(Rc::from(type_name.to_string()), vec![].into()),
                    method_name: method_name.lexeme.to_string(),
                })?;

        let (ctx, resolved_var) = method;
        let pairs = if let Some(def) = self.sys.get_struct(type_name) {
            if generics.len() != def.generic_params.len() {
                HashMap::new()
            } else {
                generics_to_map(&def.generic_params, generics)
            }
        } else {
            HashMap::new()
        };
        let ty = TypeSystem::generic_to_concrete(ctx.type_info.clone(), &pairs);
        Ok(TypedExpr {
            ty,
            kind: ExprKind::GetVar(resolved_var, ctx.name.clone()),
            line: method_name.line,
        })
    }

    pub(crate) fn resolve_instance_access(
        &mut self,
        object_typed: TypedExpr,
        member_token: &Token,
        is_safe: bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        // Unwrap optional type if safe
        let actual_ty = if is_safe {
            match &object_typed.ty {
                Type::Optional(inner) => inner.as_ref().clone(),
                _ => {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Optional(Box::new(Type::Any)),
                        found: object_typed.ty,
                        line: member_token.line,
                        message: "Cannot use safe navigation '?.' on a non-optional type.",
                    });
                }
            }
        } else {
            object_typed.ty.clone()
        };
        // Try tuple access
        if let Type::Tuple(tuple_type) = &actual_ty {
            let idx = match member_token.lexeme.parse::<u8>() {
                Ok(idx) => idx,
                Err(err) => {
                    return Err(TypeCheckerError::InvalidTupleIndex {
                        tuple_type: actual_ty,
                        index: err.to_string(),
                        line: member_token.line,
                    });
                }
            };
            if idx >= tuple_type.types.len() as u8 {
                return Err(TypeCheckerError::InvalidTupleIndex {
                    tuple_type: actual_ty,
                    index: idx.to_string(),
                    line: member_token.line,
                });
            }
            let mut expr = TypedExpr {
                ty: tuple_type.types[idx as usize].clone(),
                kind: ExprKind::GetField {
                    object: Box::new(object_typed),
                    index: idx,
                    safe: is_safe,
                },
                line: member_token.line,
            };
            if is_safe {
                expr.ty = expr.ty.wrap_in_optional();
            }
            return Ok(expr);
        }

        // Try field access
        if let Type::Struct(name, generics) = &actual_ty {
            let struct_def = self
                .sys
                .get_struct(name)
                .expect("Struct type missing definition");

            if let Some((idx, field_type)) = struct_def.get_field(member_token.lexeme) {
                let actual = TypeSystem::generic_to_concrete(
                    field_type,
                    &generics_to_map(&struct_def.generic_params, &generics),
                );

                let mut expr = TypedExpr {
                    ty: actual,
                    kind: ExprKind::GetField {
                        object: Box::new(object_typed),
                        index: idx as u8,
                        safe: is_safe,
                    },
                    line: member_token.line,
                };

                if is_safe {
                    expr.ty = expr.ty.wrap_in_optional();
                }
                return Ok(expr);
            }
        }

        // Interface Method Lookup
        if let Type::Interface(name, generics) = &actual_ty {
            let iface = self.sys.get_interface(name).unwrap();
            let Some((idx, method_ty)) = iface.methods.get(member_token.lexeme) else {
                return Err(TypeCheckerError::UndefinedMethod {
                    line: member_token.line,
                    found: Type::Interface(iface.name.clone(), generics.clone()),
                    method_name: member_token.lexeme.to_string(),
                });
            };

            let ty = match method_ty {
                Type::Function(func) => {
                    let params = func.params.iter().skip(1).cloned().collect();
                    // TODO infer generic from Struct
                    Type::new_function(params, func.return_type.clone(), func.type_params.clone())
                }
                other => other.clone(),
            };
            let ty = if is_safe { ty.wrap_in_optional() } else { ty };
            return Ok(TypedExpr {
                ty,
                kind: ExprKind::InterfaceMethodGet {
                    object: Box::new(object_typed),
                    method_index: *idx as u8,
                    safe: is_safe,
                },
                line: member_token.line,
            });
        }

        // Method Lookup
        self.handle_instance_method(member_token, &actual_ty, object_typed, is_safe)
    }

    fn handle_instance_method(
        &mut self,
        field: &Token,
        lookup_type: &Type,
        object_expr: TypedExpr,
        safe: bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let type_name = lookup_type
            .get_name()
            .ok_or(TypeCheckerError::TypeHasNoFields {
                found: lookup_type.clone(),
                line: object_expr.line,
            })?;

        let mangled_name = format!("{}.{}", type_name, field.lexeme);

        let method =
            self.scopes
                .lookup(mangled_name.as_str())
                .ok_or(TypeCheckerError::UndefinedMethod {
                    line: field.line,
                    found: lookup_type.clone(),
                    method_name: field.lexeme.to_string(),
                })?;

        let pairs = match lookup_type {
            Type::Struct(name, args) => {
                generics_to_map(&self.sys.get_struct(name).unwrap().generic_params, args)
            }
            _ => HashMap::new(),
        };

        let ty = match &method.0.type_info {
            Type::Function(func) => {
                if func.params.is_empty() || func.is_static {
                    return Err(TypeCheckerError::StaticMethodOnInstance {
                        method_name: field.lexeme.to_string(),
                        line: field.line,
                    });
                }

                let params = func
                    .params
                    .iter()
                    .skip(1)
                    .map(|(s, t)| {
                        (
                            s.to_string(),
                            TypeSystem::generic_to_concrete(t.clone(), &pairs),
                        )
                    })
                    .collect();
                let return_type = TypeSystem::generic_to_concrete(func.return_type.clone(), &pairs);

                Type::new_function(
                    params,
                    return_type,
                    func.type_params
                        .iter()
                        .filter(|s| !pairs.contains_key(*s))
                        .cloned()
                        .collect(),
                )
            }
            ty => ty.clone(),
        };

        let ty = if safe { ty.wrap_in_optional() } else { ty };

        Ok(TypedExpr {
            ty,
            kind: ExprKind::MethodGet {
                object: Box::new(object_expr),
                method: method.1,
                safe,
            },
            line: field.line,
        })
    }
}
