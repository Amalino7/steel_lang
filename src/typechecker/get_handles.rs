use crate::parser::ast::{CallArg, Expr, Literal};
use crate::token::Token;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{ExprKind, TypedExpr};
use crate::typechecker::type_system::{generics_to_map, TySys, TypeSystem};
use crate::typechecker::types::{GenericArgs, TupleType, Type};
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

        let map = generics_to_map(
            &enum_def.generic_params,
            generics,
            Some(&mut self.infer_ctx),
        );
        // TODO expected type???
        let concrete_generics = enum_def
            .generic_params
            .iter()
            .map(|s| map.get(s).unwrap().clone())
            .collect::<Vec<_>>();

        let ty = if variant_types == Type::Void {
            Type::Enum(enum_def.name.clone(), concrete_generics.into())
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
        &mut self,
        _variant_name: Symbol,
        variant_type: &Type,
        map: &HashMap<Symbol, Type>,
        inferred_args: &Vec<CallArg<'src>>,
        line: u32,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let bind_and_process = |this: &mut Self,
                                raw_params: Vec<(String, Type)>|
         -> Result<Vec<TypedExpr>, TypeCheckerError> {
            let concrete_params: Vec<(String, Type)> = raw_params
                .into_iter()
                .map(|(name, ty)| (name, TySys::generic_to_concrete(ty, map)))
                .collect();

            this.bind_arguments(
                _variant_name.as_ref(),
                &concrete_params,
                inferred_args,
                false,
                line,
            )
        };

        let val_expr = match variant_type {
            // Case: MyEnum.Struct(a: 1, b: 2)
            Type::Struct(struct_name, _) => {
                let struct_def = self
                    .sys
                    .get_struct(struct_name)
                    .expect("Enum variant points to non-existent struct");

                let concrete_struct_generics = struct_def
                    .generic_params
                    .iter()
                    .map(|t| TySys::generic_to_concrete(Type::GenericParam(t.clone()), map))
                    .collect::<Vec<_>>();

                let params = struct_def.ordered_fields.clone();

                let bound_args = bind_and_process(self, params)?;

                TypedExpr {
                    ty: Type::Struct(struct_name.clone(), Rc::new(concrete_struct_generics)),
                    kind: ExprKind::StructInit {
                        name: Box::from(struct_name.to_string()),
                        args: bound_args,
                    },
                    line,
                }
            }

            // Case: MyEnum.Tuple(1, 2)
            Type::Tuple(tuple_types) => {
                let params: Vec<(String, Type)> = tuple_types
                    .types
                    .iter()
                    .enumerate()
                    .map(|(i, t)| (i.to_string(), t.clone()))
                    .collect();

                let bound_args = bind_and_process(self, params)?;
                let concrete_tuple_types = tuple_types
                    .types
                    .iter()
                    .map(|t| TySys::generic_to_concrete(t.clone(), map))
                    .collect::<Vec<_>>();

                TypedExpr {
                    ty: Type::Tuple(Rc::new(TupleType {
                        types: concrete_tuple_types,
                    })),
                    kind: ExprKind::Tuple {
                        elements: bound_args,
                    },
                    line,
                }
            }

            // Case: MyEnum.Value(5)
            _ => {
                let params = vec![("value".to_string(), variant_type.clone())];

                let mut bound_args = bind_and_process(self, params)?;
                bound_args.pop().ok_or(TypeCheckerError::MissingArgument {
                    param_name: "value".into(),
                    callee: _variant_name.to_string(),
                    line,
                })?
            }
        };

        Ok(val_expr)
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
                generics_to_map(&def.generic_params, generics, Some(&mut self.infer_ctx))
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
        let actual_ty = object_typed
            .ty
            .unwrap_optional_safe(is_safe, member_token.line)?;

        if let Ok((idx, field_type)) = self.sys.resolve_member_type(&actual_ty, member_token) {
            let mut expr = TypedExpr {
                ty: field_type,
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

        if let Type::Interface(name, generics) = &actual_ty {
            return self.resolve_interface_method(
                name,
                generics,
                object_typed,
                member_token,
                is_safe,
            );
        }

        self.resolve_regular_method(actual_ty, object_typed, member_token, is_safe)
    }
    fn resolve_interface_method(
        &self,
        name: &Symbol,
        generics: &GenericArgs,
        object_typed: TypedExpr,
        member_token: &Token,
        is_safe: bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
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
        Ok(TypedExpr {
            ty,
            kind: ExprKind::InterfaceMethodGet {
                object: Box::new(object_typed),
                method_index: *idx as u8,
                safe: is_safe,
            },
            line: member_token.line,
        })
    }

    fn resolve_regular_method(
        &mut self,
        obj_type: Type,
        object_expr: TypedExpr,
        method_token: &Token,
        safe: bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let type_name = obj_type
            .get_name()
            .ok_or_else(|| TypeCheckerError::TypeHasNoFields {
                found: obj_type.clone(),
                line: object_expr.line,
            })?;

        // Type.method
        let mangled_name = format!("{}.{}", type_name, method_token.lexeme);

        let (ctx, resolved_var) =
            self.scopes
                .lookup(&mangled_name)
                .ok_or_else(|| TypeCheckerError::UndefinedMethod {
                    line: method_token.line,
                    found: obj_type.clone(),
                    method_name: method_token.lexeme.to_string(),
                })?;

        if let Type::Function(func) = &ctx.type_info
            && func.is_static
        {
            return Err(TypeCheckerError::StaticMethodOnInstance {
                method_name: method_token.lexeme.to_string(),
                line: method_token.line,
            });
        }

        let generics_map = self.sys.get_generics_map(&obj_type);
        let mut method_ty = match &ctx.type_info {
            Type::Function(func) => {
                let expected_self =
                    TypeSystem::generic_to_concrete(func.params[0].1.clone(), &generics_map);
                self.infer_ctx
                    .unify_types(&obj_type, &expected_self)
                    .map_err(|msg| TypeCheckerError::ComplexTypeMismatch {
                        expected: expected_self,
                        found: obj_type.clone(),
                        line: method_token.line,
                        message: msg,
                    })?;

                let params = func
                    .params
                    .iter()
                    .skip(1) // skip self
                    .map(|(name, ty)| {
                        (
                            name.clone(),
                            TypeSystem::generic_to_concrete(ty.clone(), &generics_map),
                        )
                    })
                    .collect();

                let ret = TypeSystem::generic_to_concrete(func.return_type.clone(), &generics_map);

                let remaining_generics = func
                    .type_params
                    .iter()
                    .filter(|g| !generics_map.contains_key(*g))
                    .cloned()
                    .collect();

                Type::new_function(params, ret, remaining_generics)
            }
            _ => unreachable!("Methods are only of type function"),
        };

        if safe {
            method_ty = method_ty.wrap_in_optional();
        }

        Ok(TypedExpr {
            ty: method_ty,
            kind: ExprKind::MethodGet {
                object: Box::new(object_expr),
                method: resolved_var,
                safe,
            },
            line: method_token.line,
        })
    }
    pub(crate) fn with_member_access<F>(
        &mut self,
        object_expr: &Expr<'src>,
        field: &Token,
        safe: bool,
        callback: F,
    ) -> Result<TypedExpr, TypeCheckerError>
    where
        F: FnOnce(&mut Self, TypedExpr, u8, Type) -> Result<TypedExpr, TypeCheckerError>,
    {
        let object_typed = self.check_expression(object_expr, None)?;
        let parent_type = object_typed.ty.unwrap_optional_safe(safe, field.line)?;

        let (index, field_type) = self.sys.resolve_member_type(&parent_type, field)?;
        callback(self, object_typed, index, field_type)
    }
}
