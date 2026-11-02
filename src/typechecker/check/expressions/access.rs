use crate::parser::ast::{Expr, Ident, Member};
use crate::scanner::{FileId, Span};
use crate::typechecker::core::ast::{ExprKind, TypedExpr};
use crate::typechecker::core::error::{
    Mismatch, MismatchContext, Operand, TypeCheckerError, TypeCheckerWarning, TypeRequirement,
    UndefinedMethodError,
};
use crate::typechecker::core::types::{InterfaceId, NameTypeId, Type};
use crate::typechecker::system::{TypeSystem, make_substitution_map};
use crate::typechecker::{TypeChecker, similarity};
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_get(
        &mut self,
        object: &Expr<'src>,
        member: &Member<'src>,
        safe: Option<Span>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let object_typed = self.check_expression(object, &Type::Unknown);
        if object_typed.ty == Type::Error {
            return Ok(TypedExpr::new_blank(object_typed.span));
        }
        if let Type::Metatype(id, generics) = &object_typed.ty {
            if let Some(span) = safe {
                self.warnings
                    .push(TypeCheckerWarning::SafeAccessOnNonOptional { span })
            }
            return match member {
                Member::Named(name) => self.resolve_static_access(*id, name, generics),
                Member::Index(index) => Err(TypeCheckerError::TupleIndexOnNonTuple {
                    found: object_typed.ty.clone(),
                    span: index.span,
                }),
            };
        }

        self.resolve_instance_access(object_typed, member, safe)
    }

    pub(crate) fn check_set(
        &mut self,
        object: &Expr<'src>,
        member: &Member,
        value: &Expr<'src>,
        safe: Option<Span>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        self.with_member_access(
            object,
            member,
            safe,
            |this, typed_obj, index, field_type, safe| {
                let typed_value = this.coerce_expression(
                    value,
                    &field_type,
                    MismatchContext::Field {
                        name: member.to_string(),
                    },
                    None,
                );
                Ok(TypedExpr {
                    ty: field_type,
                    span: typed_value.span.merge(typed_obj.span),
                    kind: ExprKind::SetField {
                        object: Box::new(typed_obj),
                        index,
                        value: Box::new(typed_value),
                        safe,
                    },
                })
            },
        )
    }

    pub(crate) fn check_get_index(
        &mut self,
        expr: &Expr<'src>,
        safe: Option<Span>,
        object: &Expr<'src>,
        index: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let object_typed = self.check_expression(object, &Type::Unknown);
        let (parent_type, safe) = object_typed
            .ty
            .unwrap_optional_safe(safe, &mut self.warnings);

        // Map indexing: always returns V? (Optional)
        if let (Some(key_ty), Some(val_ty)) = (
            parent_type.map_key(self.sys).cloned(),
            parent_type.map_value(self.sys).cloned(),
        ) {
            let index_typed =
                self.coerce_expression(index, &key_ty, MismatchContext::IndexValue, None);

            let mut ty = Type::Optional(Box::new(val_ty));
            if safe {
                ty = ty.wrap_in_optional();
            }

            return Ok(TypedExpr {
                ty,
                span: expr.span,
                kind: ExprKind::MapGet {
                    object: Box::new(object_typed),
                    key: Box::new(index_typed),
                    safe,
                },
            });
        }

        let inner = parent_type.list_element(self.sys).cloned().ok_or_else(|| {
            TypeCheckerError::OperatorConstraint {
                operator: "[]",
                operand: Operand::Lhs,
                found: parent_type.clone(),
                requirement: TypeRequirement::Structural("List or Map"),
                span: object_typed.span,
            }
        })?;

        let index_typed = self.check_expression(index, &Type::Number);
        self.check_operand(
            Type::Number,
            &index_typed.ty,
            Operand::Rhs,
            "[]",
            index_typed.span,
        );

        let mut ty = inner;
        if safe {
            ty = ty.wrap_in_optional();
        }

        Ok(TypedExpr {
            ty,
            span: expr.span,
            kind: ExprKind::GetIndex {
                object: Box::new(object_typed),
                index: Box::new(index_typed),
                safe,
            },
        })
    }

    pub(crate) fn check_set_index(
        &mut self,
        expr: &Expr,
        safe: Option<Span>,
        object: &Expr<'src>,
        index: &Expr<'src>,
        value: &Expr<'src>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let object_typed = self.check_expression(object, &Type::Unknown);
        let (parent_type, safe) = object_typed
            .ty
            .unwrap_optional_safe(safe, &mut self.warnings);

        // Map set-indexing: upsert key -> value
        if let (Some(key_ty), Some(val_ty)) = (
            parent_type.map_key(self.sys).cloned(),
            parent_type.map_value(self.sys).cloned(),
        ) {
            let index_typed =
                self.coerce_expression(index, &key_ty, MismatchContext::IndexValue, None);
            let value_typed =
                self.coerce_expression(value, &val_ty, MismatchContext::IndexValue, None);

            return Ok(TypedExpr {
                ty: val_ty,
                span: expr.span,
                kind: ExprKind::MapSet {
                    object: Box::new(object_typed),
                    key: Box::new(index_typed),
                    value: Box::new(value_typed),
                    safe,
                },
            });
        }

        let inner = parent_type.list_element(self.sys).cloned().ok_or_else(|| {
            TypeCheckerError::OperatorConstraint {
                operator: "[]",
                operand: Operand::Lhs,
                found: parent_type.clone(),
                requirement: TypeRequirement::Structural("List or Map"),
                span: object_typed.span,
            }
        })?;

        let index_typed = self.check_expression(index, &Type::Number);

        self.check_operand(
            Type::Number,
            &index_typed.ty,
            Operand::Rhs,
            "[]",
            index_typed.span,
        );
        let value_typed = self.coerce_expression(value, &inner, MismatchContext::IndexValue, None);

        Ok(TypedExpr {
            ty: inner,
            span: expr.span,
            kind: ExprKind::SetIndex {
                object: Box::new(object_typed),
                index: Box::new(index_typed),
                value: Box::new(value_typed),
                safe,
            },
        })
    }

    /// A tuple index must select an element. A name selects a field, or else a method.
    pub(crate) fn resolve_instance_access(
        &mut self,
        object_typed: TypedExpr,
        member: &Member,
        safe: Option<Span>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let (actual_ty, safe) = object_typed
            .ty
            .unwrap_optional_safe(safe, &mut self.warnings);

        let field = resolve_member_type(&actual_ty, member, self.sys, self.file_id);
        let member_token = match (member, field) {
            (Member::Index(_), Err(err)) => return Err(err),
            (_, Ok((idx, field_type))) => {
                if let Member::Named(name) = member {
                    self.check_field_access(&actual_ty, idx, name);
                }
                let mut expr = TypedExpr {
                    ty: field_type,
                    span: object_typed.span.merge(member.span()),
                    kind: ExprKind::GetField {
                        object: Box::new(object_typed),
                        index: idx,
                        safe,
                    },
                };
                if safe {
                    expr.ty = expr.ty.wrap_in_optional();
                }
                return Ok(expr);
            }
            (Member::Named(name), Err(_)) => name,
        };

        if let Type::Interface(id) = &actual_ty {
            return self.resolve_interface_method(*id, object_typed, member_token, safe);
        }

        self.resolve_regular_method(actual_ty, object_typed, member_token, safe)
    }
    fn resolve_interface_method(
        &self,
        id: InterfaceId,
        object_typed: TypedExpr,
        member_token: &Ident,
        is_safe: bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let iface = self.sys.get_interface(id);
        let Some((idx, method_ty)) = iface.methods.get(member_token.name) else {
            let method_names: Vec<&str> = iface.methods.keys().map(|s| s.as_str()).collect();
            let suggestions = similarity::find_similar(member_token.name, method_names, 3);
            return Err(TypeCheckerError::UndefinedMethod(Box::new(
                UndefinedMethodError {
                    span: member_token.span,
                    found: Type::Interface(id),
                    method_name: member_token.name.into(),
                    type_origin: Some(iface.origin),
                    suggestions,
                },
            )));
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
        method_token: &Ident,
        safe: bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let type_id = obj_type
            .get_ty_id()
            .ok_or_else(|| TypeCheckerError::TypeHasNoFields {
                found: obj_type.clone(),
                span: object_expr.span,
            })?;

        let lookup_result = self.lookup_method(type_id, method_token.name);

        let Some(method) = lookup_result else {
            let mut candidates = self.method_names_for_type(type_id);
            // Add field names if this is a struct type
            if let NameTypeId::Struct(id) = type_id {
                let def = self.sys.get_struct(id);
                candidates.extend(def.visible_field_names(self.file_id).cloned());
            }

            let suggestions = similarity::find_similar(
                method_token.name,
                candidates.iter().map(|s| s.as_ref()),
                3,
            );
            let type_origin = self.sys.get_origin(type_id);
            return Err(TypeCheckerError::UndefinedMethod(Box::new(
                UndefinedMethodError {
                    span: method_token.span,
                    found: obj_type.clone(),
                    method_name: method_token.name.into(),
                    type_origin,
                    suggestions,
                },
            )));
        };

        self.check_method_access(method, method_token);
        let method_info = self.sys.get_method_info(method.target);

        let definition_span = method_info.origin;
        let method_type = method_info.func_type.clone();
        let location = method_info.location.clone();
        let impl_count = method_info.impl_generic_count;

        let Type::Function(func) = &method_type else {
            unreachable!(
                "Method should be of type function {}",
                method_type.display_type(self.sys)
            )
        };

        if func.is_static() {
            return Err(TypeCheckerError::StaticMethodOnInstance {
                method_name: method_token.name.to_string(),
                span: method_token.span,
            });
        }

        let impl_params = &func.type_params[0..impl_count];
        let fresh_generics = self.infer_ctx.fresh_args(impl_params, &[]);
        let impl_map = make_substitution_map(impl_params, &fresh_generics);

        let method_with_fresh = method_type.generic_to_concrete(&impl_map);

        let Type::Function(fresh_func) = &method_with_fresh else {
            unreachable!()
        };
        let self_param_ty = fresh_func.params[0].1.clone();
        self.infer_ctx
            .unify_types(&self_param_ty, &obj_type)
            .map_err(|unif_err| TypeCheckerError::TypeMismatch {
                mismatch: Box::new(Mismatch::from(unif_err)),
                context: MismatchContext::Generic,
                primary_span: method_token.span,
                defined_at: None,
            })?;

        let result_type = self.infer_ctx.substitute(&method_with_fresh);

        // Remove the 'self' parameter from the resolved method type.
        let mut method_ty = if let Type::Function(mut func) = result_type {
            let func_inner = Rc::make_mut(&mut func);
            func_inner.params.remove(0);
            Type::Function(func)
        } else {
            unreachable!("Method should be of type function")
        };

        if safe {
            method_ty = method_ty.wrap_in_optional();
        }

        Ok(TypedExpr {
            ty: method_ty,
            span: object_expr.span.merge(method_token.span),
            kind: ExprKind::MethodGet {
                object: Box::new(object_expr),
                method: location,
                origin: definition_span,
                safe,
            },
        })
    }
    pub(crate) fn check_field_access(&mut self, ty: &Type, idx: u8, field: &Ident) {
        let Type::Struct(id, _) = ty else { return };
        let def = self.sys.get_struct(*id);
        if def.is_field_public(idx as usize) || def.origin.file_id == self.file_id {
            return;
        }
        let error = TypeCheckerError::PrivateField {
            struct_name: def.name.clone(),
            field_name: field.name.into(),
            module: self.module_graph.module_name_of_file(def.origin.file_id),
            span: field.span,
            definition: def.field_span(idx as usize),
        };
        self.report(error);
    }

    pub(crate) fn with_member_access<F>(
        &mut self,
        object_expr: &Expr<'src>,
        member: &Member,
        safe: Option<Span>,
        callback: F,
    ) -> Result<TypedExpr, TypeCheckerError>
    where
        F: FnOnce(&mut Self, TypedExpr, u8, Type, bool) -> Result<TypedExpr, TypeCheckerError>,
    {
        let object_typed = self.check_expression(object_expr, &Type::Unknown);
        let (parent_type, safe) = object_typed
            .ty
            .unwrap_optional_safe(safe, &mut self.warnings);

        let (index, field_type) =
            resolve_member_type(&parent_type, member, self.sys, self.file_id)?;
        if let Member::Named(name) = member {
            self.check_field_access(&parent_type, index, name);
        }
        callback(self, object_typed, index, field_type, safe)
    }
}

/// The slot and type of a tuple element or struct field. A name on a tuple, or anything on a type without
/// fields, is `TypeHasNoFields`.
fn resolve_member_type(
    parent_type: &Type,
    member: &Member,
    sys: &TypeSystem,
    file_id: FileId,
) -> Result<(u8, Type), TypeCheckerError> {
    let field = match member {
        Member::Index(index) => return resolve_tuple_index(parent_type, index.node, index.span),
        Member::Named(name) => name,
    };
    match parent_type {
        Type::Struct(id, generics) => {
            let struct_def = sys.get_struct(*id);

            let (field_id, ty) = struct_def.get_field(field.name, generics).ok_or_else(|| {
                let field_names: Vec<&str> = struct_def
                    .visible_field_names(file_id)
                    .map(|name| name.as_ref())
                    .collect();
                let suggestions = similarity::find_similar(field.name, field_names, 3);
                TypeCheckerError::UndefinedField {
                    struct_name: struct_def.name.clone(),
                    field_name: field.name.into(),
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

/// The slot and type of element `index` of `parent_type`, which must be a tuple.
fn resolve_tuple_index(
    parent_type: &Type,
    index: usize,
    span: Span,
) -> Result<(u8, Type), TypeCheckerError> {
    let Type::Tuple(tuple_type) = parent_type else {
        return Err(TypeCheckerError::TupleIndexOnNonTuple {
            found: parent_type.clone(),
            span,
        });
    };
    match (u8::try_from(index), tuple_type.types.get(index)) {
        (Ok(slot), Some(ty)) => Ok((slot, ty.clone())),
        _ => Err(TypeCheckerError::InvalidTupleIndex {
            tuple_type: parent_type.clone(),
            index,
            span,
        }),
    }
}
