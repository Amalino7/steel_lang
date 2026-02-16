use crate::parser::ast::Expr;
use crate::scanner::Token;
use crate::typechecker::core::ast::{ExprKind, TypedExpr};
use crate::typechecker::core::error::{TypeCheckerError, TypeCheckerWarning};
use crate::typechecker::core::types::{GenericArgs, Type};
use crate::typechecker::system::TypeSystem;
use crate::typechecker::{similarity, Symbol, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_get(
        &mut self,
        expr: &Expr<'src>,
        object: &Expr<'src>,
        field: &Token<'src>,
        safe: &bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let object_typed = self.check_expression(object, &Type::Unknown)?;
        if let Type::Metatype(name, generics) = &object_typed.ty {
            if *safe {
                self.warnings
                    .push(TypeCheckerWarning::SafeAccessOnNonOptional { span: expr.span() })
            }
            return self.resolve_static_access(name, field, generics);
        }

        self.resolve_instance_access(object_typed, field, *safe)
    }

    pub(crate) fn check_set(
        &mut self,
        object: &Expr<'src>,
        field: &Token,
        value: &Expr<'src>,
        safe: &bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        self.with_member_access(
            object,
            field,
            *safe,
            |this, typed_obj, index, field_type| {
                let typed_value = this.coerce_expression(value, &field_type)?;
                Ok(TypedExpr {
                    ty: field_type,
                    span: typed_value.span.merge(typed_obj.span),
                    kind: ExprKind::SetField {
                        object: Box::new(typed_obj),
                        index,
                        value: Box::new(typed_value),
                        safe: *safe,
                    },
                })
            },
        )
    }

    pub(crate) fn check_get_index(
        &mut self,
        expr: &Expr<'src>,
        safe: &bool,
        object: &Expr<'src>,
        index: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let object_typed = self.check_expression(object, &Type::Unknown)?;
        let parent_type = object_typed
            .ty
            .unwrap_optional_safe(*safe, object_typed.span)?;

        let Type::List(inner) = parent_type else {
            return Err(TypeCheckerError::TypeMismatch {
                expected: Type::List(Box::new(Type::Any)),
                found: parent_type,
                span: object_typed.span,
                message: "Indexing is only supported on lists.",
            });
        };

        let index_typed = self.coerce_expression(index, &Type::Number)?;
        let mut ty = *inner;
        if *safe {
            ty = ty.wrap_in_optional();
        }

        Ok(TypedExpr {
            ty,
            span: expr.span(),
            kind: ExprKind::GetIndex {
                object: Box::new(object_typed),
                index: Box::new(index_typed),
                safe: *safe,
            },
        })
    }

    pub(crate) fn check_set_index(
        &mut self,
        expr: &Expr<'src>,
        safe: &bool,
        object: &Expr<'src>,
        index: &Expr<'src>,
        value: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let object_typed = self.check_expression(object, &Type::Unknown)?;
        let parent_type = object_typed
            .ty
            .unwrap_optional_safe(*safe, object_typed.span)?;

        let Type::List(inner) = parent_type else {
            return Err(TypeCheckerError::TypeMismatch {
                expected: Type::List(Box::new(Type::Any)),
                found: parent_type,
                span: object_typed.span,
                message: "Indexing is only supported on lists.",
            });
        };

        let index_typed = self.coerce_expression(index, &Type::Number)?;
        let value_typed = self.coerce_expression(value, &inner)?;

        Ok(TypedExpr {
            ty: *inner,
            span: expr.span(),
            kind: ExprKind::SetIndex {
                object: Box::new(object_typed),
                index: Box::new(index_typed),
                value: Box::new(value_typed),
                safe: *safe,
            },
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
            .unwrap_optional_safe(is_safe, member_token.span)?;

        if let Ok((idx, field_type)) = resolve_member_type(&actual_ty, member_token, &self.sys) {
            let mut expr = TypedExpr {
                ty: field_type,
                span: object_typed.span.merge(member_token.span),
                kind: ExprKind::GetField {
                    object: Box::new(object_typed),
                    index: idx,
                    safe: is_safe,
                },
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
            let method_names: Vec<&str> = iface.methods.keys().map(|s| s.as_str()).collect();
            let suggestions = similarity::find_similar(member_token.lexeme, method_names, 3);
            return Err(TypeCheckerError::UndefinedMethod {
                span: member_token.span,
                found: Type::Interface(iface.name.clone(), generics.clone()),
                method_name: member_token.lexeme.to_string(),
                type_origin: Some(iface.origin),
                suggestions,
            });
        };

        let ty = match method_ty {
            Type::Function(func) => {
                let params = func.params.iter().skip(1).cloned().collect();
                Type::new_function(params, func.return_type.clone(), func.type_params.clone())
            }
            other => other.clone(),
        };
        let ty = if is_safe { ty.wrap_in_optional() } else { ty };
        Ok(TypedExpr {
            ty,
            span: object_typed.span.merge(member_token.span),
            kind: ExprKind::InterfaceMethodGet {
                object: Box::new(object_typed),
                method_index: *idx as u8,
                safe: is_safe,
            },
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
                span: object_expr.span,
            })?;

        // Type.method
        let mangled_name = format!("{}.{}", type_name, method_token.lexeme);

        let methods = self.scopes.get_methods_for_type(type_name);
        let lookup_result = self.scopes.lookup(&mangled_name);
        let (ctx, resolved_var) = lookup_result.ok_or_else(|| {
            // Combine methods and fields for suggestions (user might have meant either)
            let mut candidates: Vec<String> = methods.clone();

            // Add field names if this is a struct type
            if let Some(struct_def) = self.sys.get_struct(type_name) {
                candidates.extend(struct_def.fields.keys().map(|s| s.to_string()));
            }

            let suggestions = crate::typechecker::similarity::find_similar(
                method_token.lexeme,
                candidates.iter().map(|s| s.as_str()),
                3,
            );
            TypeCheckerError::UndefinedMethod {
                span: method_token.span,
                found: obj_type.clone(),
                method_name: method_token.lexeme.to_string(),
                type_origin: None, // TODO: track type definition span
                suggestions,
            }
        })?;

        if let Type::Function(func) = &ctx.type_info
            && func.is_static()
        {
            return Err(TypeCheckerError::StaticMethodOnInstance {
                method_name: method_token.lexeme.to_string(),
                span: method_token.span,
            });
        }

        let generics_map = self.sys.get_generics_map(&obj_type);
        let mut method_ty = match &ctx.type_info {
            Type::Function(func) => {
                let expected_self = func.params[0].1.clone().generic_to_concrete(&generics_map);
                self.infer_ctx
                    .unify_types(&obj_type, &expected_self)
                    .map_err(|msg| TypeCheckerError::ComplexTypeMismatch {
                        expected: expected_self,
                        found: obj_type.clone(),
                        span: method_token.span,
                        message: msg.into(),
                    })?;

                let params = func
                    .params
                    .iter()
                    .skip(1) // skip self
                    .map(|(name, ty)| (name.clone(), ty.clone().generic_to_concrete(&generics_map)))
                    .collect();

                let ret = func.return_type.clone().generic_to_concrete(&generics_map);

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
            span: object_expr.span.merge(method_token.span),
            kind: ExprKind::MethodGet {
                object: Box::new(object_expr),
                method: resolved_var,
                safe,
            },
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
        let object_typed = self.check_expression(object_expr, &Type::Unknown)?;
        let parent_type = object_typed.ty.unwrap_optional_safe(safe, field.span)?;

        let (index, field_type) = resolve_member_type(&parent_type, field, &self.sys)?;
        callback(self, object_typed, index, field_type)
    }
}

fn resolve_member_type(
    parent_type: &Type,
    field: &Token,
    sys: &TypeSystem,
) -> Result<(u8, Type), TypeCheckerError> {
    match parent_type {
        Type::Tuple(tuple_type) => {
            let idx =
                field
                    .lexeme
                    .parse::<u8>()
                    .map_err(|_| TypeCheckerError::InvalidTupleIndex {
                        tuple_type: parent_type.clone(),
                        index: field.lexeme.to_string(),
                        span: field.span,
                    })?;

            if idx as usize >= tuple_type.types.len() {
                return Err(TypeCheckerError::InvalidTupleIndex {
                    tuple_type: parent_type.clone(),
                    index: idx.to_string(),
                    span: field.span,
                });
            }

            Ok((idx, tuple_type.types[idx as usize].clone()))
        }
        Type::Struct(name, generics) => {
            let struct_def = sys
                .get_struct(name)
                .expect("Struct def missing after type check");

            let (field_id, ty) = struct_def
                .get_field(field.lexeme, generics)
                .ok_or_else(|| {
                    let field_names: Vec<&str> =
                        struct_def.fields.keys().map(|s| s.as_ref()).collect();
                    let suggestions = similarity::find_similar(field.lexeme, field_names, 3);
                    TypeCheckerError::UndefinedField {
                        struct_name: struct_def.name.to_string(),
                        field_name: field.lexeme.to_string(),
                        span: field.span,
                        struct_origin: Some(struct_def.origin),
                        suggestions,
                    }
                })?;

            Ok((field_id as u8, ty))
        }
        _ => Err(TypeCheckerError::TypeHasNoFields {
            found: parent_type.clone(),
            span: field.span,
        }),
    }
}
