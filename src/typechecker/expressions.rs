use crate::compiler::analysis::ResolvedVar;
use crate::compiler::analysis::ResolvedVar::Global;
use crate::parser::ast::{Expr, Literal};
use crate::scanner::TokenType;
use crate::typechecker::error::TypeCheckerError::AssignmentToCapturedVariable;
use crate::typechecker::error::{Recoverable, TypeCheckerError};
use crate::typechecker::type_ast::{ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::type_system::{generics_to_map, TySys, TypeSystem};
use crate::typechecker::types::{TupleType, Type};
use crate::typechecker::{FunctionContext, Symbol, TypeChecker};
use std::collections::HashMap;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_expression(
        &mut self,
        expr: &Expr<'src>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        match expr {
            Expr::TypeSpecialization {
                callee,
                generics: generics_provided,
            } => {
                let mut callee_typed = self.check_expression(callee, &Type::Unknown)?;
                let generic_args = generics_provided
                    .iter()
                    .map(|t| Type::from_ast(t, &self.sys))
                    .collect::<Result<Vec<_>, _>>()?;

                match &mut callee_typed.ty {
                    Type::Function(func) => {
                        if func.type_params.len() != generic_args.len() {
                            return Err(TypeCheckerError::InvalidGenericSpecification {
                                span: callee_typed.span,
                                message: format!(
                                    "Expected {} generic arguments but got {}!",
                                    func.type_params.len(),
                                    generic_args.len(),
                                ),
                            });
                        }
                        if func.type_params.is_empty() {
                            return Err(TypeCheckerError::InvalidGenericSpecification {
                                span: callee_typed.span,
                                message: "Cannot specialize generic on function without generics!"
                                    .to_string(),
                            });
                        }
                        let map = generics_to_map(&func.type_params, &generic_args, None);
                        let new_ty = TypeSystem::generic_to_concrete(callee_typed.ty, &map);
                        callee_typed.ty = new_ty;
                        callee_typed.span = expr.span();
                        Ok(callee_typed)
                    }
                    Type::Metatype(type_name, generics) => {
                        if !generics.is_empty() {
                            return Err(TypeCheckerError::InvalidGenericSpecification {
                                span: callee_typed.span,
                                message: "Cannot specialize generic more than once!".to_string(),
                            });
                        }
                        let actual_generic_count = self.sys.get_generics_count(type_name);
                        if generic_args.len() != actual_generic_count {
                            Err(TypeCheckerError::InvalidGenericSpecification {
                                span: callee_typed.span,
                                message: format!(
                                    "Expected {} but got {} generic arguments!",
                                    actual_generic_count,
                                    generic_args.len(),
                                ),
                            })
                        } else {
                            Rc::get_mut(generics).unwrap().extend(generic_args); // Shouldn't fail
                            callee_typed.span = expr.span();
                            Ok(callee_typed)
                        }
                    }
                    _ => Err(TypeCheckerError::InvalidGenericSpecification {
                        span: callee_typed.span,
                        message: "Cannot specialize non-generic type!".to_string(),
                    }),
                }
            }
            Expr::Is {
                expression,
                type_name,
            } => {
                let target = self.check_expression(expression, &Type::Unknown)?;
                let Type::Enum(enum_name, _) = &target.ty else {
                    return Err(TypeCheckerError::InvalidIsUsage {
                        span: type_name.span,
                        message: "Is can only be used on enum types.",
                    });
                };
                let enum_def = self
                    .sys
                    .get_enum(enum_name.as_ref())
                    .expect("Invalid enum Type return!");

                if !enum_def.variants.contains_key(type_name.lexeme) {
                    return Err(TypeCheckerError::InvalidIsUsage {
                        span: type_name.span,
                        message: "Enum variant does not exist.",
                    });
                }
                let variant_idx = enum_def.variants.get(type_name.lexeme).unwrap();
                Ok(TypedExpr {
                    ty: Type::Boolean,
                    kind: ExprKind::Is {
                        target: Box::new(target),
                        variant_idx: *variant_idx as u16,
                    },
                    span: expr.span(),
                })
            }
            Expr::Tuple { elements } => {
                let mut typed_elements = Vec::with_capacity(elements.len());
                let mut type_vec = vec![];

                let expected_types = match *expected {
                    Type::Tuple(ref t) => Some(&t.types),
                    _ => None,
                };

                for (idx, element) in elements.iter().enumerate() {
                    let target_inner = expected_types.and_then(|v| v.get(idx));
                    let el =
                        self.coerce_expression(element, target_inner.unwrap_or(&Type::Unknown))?;
                    type_vec.push(el.ty.clone());
                    typed_elements.push(el);
                }

                let ty = Type::Tuple(Rc::new(TupleType { types: type_vec }));
                Ok(TypedExpr {
                    ty,
                    kind: ExprKind::Tuple {
                        elements: typed_elements,
                    },
                    span: expr.span(),
                })
            }
            Expr::Unary {
                operator,
                expression,
            } if operator.token_type == TokenType::Try => {
                // TODO add suggested type
                let mut typed_expr = self.check_expression(expression, &Type::Unknown)?;
                if let Type::Enum(name, instance) = &mut typed_expr.ty
                    && name.as_ref() == "Result"
                {
                    let enum_def = self.sys.get_enum(name.as_ref()).unwrap();
                    let map = generics_to_map(&enum_def.generic_params, instance, None);
                    let ok_type = TypeSystem::generic_to_concrete(
                        enum_def.ordered_variants[0].1.clone(),
                        &map,
                    );
                    let err_type = TypeSystem::generic_to_concrete(
                        enum_def.ordered_variants[1].1.clone(),
                        &map,
                    );

                    if let FunctionContext::Function(func_return_type, _) =
                        self.current_function.clone()
                    {
                        let provided_err =
                            Type::Enum(name.clone(), vec![Type::Never, err_type].into());
                        let ok = self.infer_ctx.unify_types(&func_return_type, &provided_err);
                        if ok.is_err() {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: func_return_type,
                                found: provided_err,
                                span: typed_expr.span,
                                message: "The Result type propagate by try must be compatible with the function return type.",
                            });
                        }
                    } else {
                        return Err(TypeCheckerError::InvalidReturnOutsideFunction {
                            span: operator.span,
                        });
                    }

                    Ok(TypedExpr {
                        ty: ok_type,
                        kind: ExprKind::Try {
                            operand: Box::new(typed_expr),
                        },
                        span: expr.span(),
                    })
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Enum("Result".into(), vec![Type::Any, Type::Any].into()),
                        found: typed_expr.ty,
                        span: operator.span,
                        message: "Try works only on the Result enum.",
                    })
                }
            }
            Expr::Unary {
                operator,
                expression,
            } => self.check_unary_expression(operator, expression, expected),
            Expr::Binary {
                operator,
                left,
                right,
            } => self.check_binary_expression(operator, left, right),

            Expr::Variable { name } => {
                let var = self.scopes.lookup(name.lexeme);
                if let Some((ctx, resolved)) = var {
                    Ok(TypedExpr {
                        ty: ctx.type_info.clone(),
                        kind: ExprKind::GetVar(resolved, ctx.name.clone()),
                        span: name.span,
                    })
                } else if let Some(type_name) = self.sys.get_owned_type_name(name.lexeme) {
                    Ok(TypedExpr {
                        ty: Type::Metatype(type_name.clone(), vec![].into()),
                        kind: ExprKind::GetVar(Global(0), type_name),
                        span: name.span,
                    })
                } else {
                    Err(TypeCheckerError::UndefinedVariable {
                        name: name.lexeme.to_string(),
                        span: name.span,
                    })
                }
            }
            Expr::Grouping { expression } => self.check_expression(expression, expected),
            Expr::Literal { literal, span } => {
                let ty = match literal {
                    Literal::Number(_) => Type::Number,
                    Literal::String(_) => Type::String,
                    Literal::Boolean(_) => Type::Boolean,
                    Literal::Void => Type::Void,
                    Literal::Nil => Type::Nil,
                };

                Ok(TypedExpr {
                    ty,
                    kind: ExprKind::Literal(literal.clone()),
                    span: *span,
                })
            }
            Expr::Assignment { identifier, value } => {
                let var_lookup = self.scopes.lookup(identifier.lexeme);

                if let Some((ctx, resolved)) = var_lookup {
                    if let ResolvedVar::Closure(_) = &resolved {
                        return Err(AssignmentToCapturedVariable {
                            name: ctx.name.to_string(),
                            span: identifier.span,
                        });
                    }
                    let expected = ctx.type_info.clone();
                    let coerced_value = self.coerce_expression(value, &expected)?;

                    Ok(TypedExpr {
                        ty: coerced_value.ty.clone(),
                        kind: ExprKind::Assign {
                            target: resolved,
                            value: Box::new(coerced_value),
                        },
                        span: expr.span(),
                    })
                } else {
                    Err(TypeCheckerError::UndefinedVariable {
                        name: identifier.lexeme.to_string(),
                        span: identifier.span,
                    })
                }
            }
            Expr::Logical {
                operator,
                left,
                right,
            } if operator.token_type == TokenType::QuestionQuestion => {
                let wrapped_expected = if *expected == Type::Unknown {
                    Type::Unknown
                } else {
                    expected.clone().wrap_in_optional()
                };

                let left_typed = self.check_expression(left, &wrapped_expected)?;
                let right_typed = self.check_expression(right, expected)?;
                let left_inner = match &left_typed.ty {
                    Type::Optional(inner) => inner.as_ref(),
                    _ => {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Optional(Box::new(Type::Any)),
                            found: left_typed.ty,
                            span: left_typed.span,
                            message: "Cannot coalesce non-optional type.",
                        });
                    }
                };
                if &right_typed.ty == left_inner {
                    Ok(TypedExpr {
                        ty: left_inner.clone(),
                        kind: ExprKind::Logical {
                            left: Box::new(left_typed),
                            operator: LogicalOp::Coalesce,
                            right: Box::new(right_typed),
                            typed_refinements: vec![],
                        },
                        span: expr.span(),
                    })
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: left_inner.clone(),
                        found: right_typed.ty,
                        span: operator.span,
                        message: "Cannot coalesce different types.",
                    })
                }
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => self.check_logical_expression(operator, left, right),
            Expr::Call {
                callee,
                arguments,
                safe,
            } => {
                let mut callee_typed = self.check_expression(callee, &Type::Unknown)?;
                // Handle Struct constructor
                if let Type::Metatype(name, generics) = &callee_typed.ty
                    && let Some(struct_def) = self.sys.get_struct(name)
                {
                    let map: HashMap<Symbol, Type> = generics_to_map(
                        &struct_def.generic_params,
                        generics,
                        Some(&mut self.infer_ctx),
                    );

                    let abstracted = struct_def
                        .ordered_fields
                        .iter()
                        .map(|(s, ty)| (s.clone(), TySys::generic_to_concrete(ty.clone(), &map)))
                        .collect::<Vec<_>>();

                    let owned_name = struct_def.name.clone();
                    let bound_args =
                        self.bind_arguments(callee.span(), &abstracted, arguments, false)?;

                    let type_args = abstracted
                        .iter()
                        .map(|(_, ty)| self.infer_ctx.substitute(ty))
                        .collect::<Vec<_>>();

                    return Ok(TypedExpr {
                        ty: Type::Struct(owned_name.clone(), Rc::new(type_args)),
                        kind: ExprKind::StructInit {
                            name: Box::from(owned_name.to_string()),
                            args: bound_args,
                        },
                        span: expr.span(),
                    });
                }

                if let Type::Metatype(name, generics) = &callee_typed.ty
                    && let Some(enum_def) = self.sys.get_enum(name)
                    && let ExprKind::EnumInit {
                        variant_idx, value, ..
                    } = &mut callee_typed.kind
                {
                    let map = generics_to_map(
                        &enum_def.generic_params,
                        generics,
                        Some(&mut self.infer_ctx),
                    );
                    let (variant_name, variant_type) =
                        &enum_def.ordered_variants[*variant_idx as usize];
                    let concrete_generics = enum_def
                        .generic_params
                        .iter()
                        .map(|s| map.get(s).unwrap().clone())
                        .collect::<Vec<_>>();

                    let init_expr = self.handle_enum_call(
                        variant_name.clone(),
                        &variant_type.clone(),
                        &map,
                        arguments,
                        callee.span(),
                    )?;

                    *value = Box::from(init_expr);
                    let result = Type::Enum(name.clone(), Rc::new(concrete_generics));
                    callee_typed.ty = self.infer_ctx.substitute(&result);
                    callee_typed.span = expr.span();

                    return Ok(callee_typed);
                }

                let safe = if let ExprKind::MethodGet { safe, .. } = callee_typed.kind {
                    safe
                } else if let ExprKind::InterfaceMethodGet { safe, .. } = callee_typed.kind {
                    safe
                } else {
                    *safe
                };

                let lookup_type = callee_typed
                    .ty
                    .unwrap_optional_safe(safe, callee_typed.span)?;

                // Check for Normal Function Call
                match lookup_type {
                    Type::Function(func) => {
                        let map: HashMap<Symbol, Type> =
                            generics_to_map(&func.type_params, &[], Some(&mut self.infer_ctx));

                        let Type::Function(func) =
                            TypeSystem::generic_to_concrete(Type::Function(func.clone()), &map)
                        else {
                            panic!("Should have errored earlier");
                        };

                        let bound_args = self.bind_arguments(
                            callee.span(),
                            &func.params,
                            arguments,
                            func.is_vararg,
                        )?;
                        let ret_type = if safe {
                            func.return_type.clone().wrap_in_optional()
                        } else {
                            func.return_type.clone()
                        };

                        let uninferred: Vec<_> = map
                            .iter()
                            .filter(|(_, v)| **v == Type::Unknown)
                            .map(|(name, _)| name.to_string())
                            .collect();
                        if !uninferred.is_empty() {
                            return Err(TypeCheckerError::CannotInferType {
                                span: callee.span(),
                                uninferred_generics: uninferred,
                            });
                        }
                        let ret_type = self.infer_ctx.substitute(&ret_type);

                        Ok(TypedExpr {
                            ty: ret_type,
                            span: expr.span(),
                            kind: ExprKind::Call {
                                callee: Box::new(callee_typed),
                                arguments: bound_args,
                                safe,
                            },
                        })
                    }
                    _ => Err(TypeCheckerError::CalleeIsNotCallable {
                        found: callee_typed.ty,
                        span: callee_typed.span,
                    }),
                }
            }

            Expr::Get {
                object,
                field,
                safe,
            } => {
                //TODO Ignores safe static access probably should be a warning
                let object_typed = self.check_expression(object, &Type::Unknown)?;
                if let Type::Metatype(name, generics) = &object_typed.ty {
                    return self.resolve_static_access(name, field, generics);
                }

                self.resolve_instance_access(object_typed, field, *safe)
            }
            Expr::Set {
                object,
                field,
                value,
                safe,
            } => self.with_member_access(
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
            ),

            Expr::ForceUnwrap {
                expression,
                operator,
            } => {
                let expected_opt = if *expected == Type::Unknown {
                    Type::Unknown
                } else {
                    Type::Optional(Box::new(expected.clone()))
                };

                let expr_typed = self.check_expression(expression, &expected_opt)?;
                match expr_typed.ty.clone() {
                    Type::Optional(inner) => Ok(TypedExpr {
                        ty: *inner,
                        kind: ExprKind::Unary {
                            operator: UnaryOp::Unwrap,
                            operand: Box::new(expr_typed),
                        },
                        span: expr.span(),
                    }),
                    _ => Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Optional(Box::new(Type::Any)),
                        found: expr_typed.ty,
                        span: operator.span,
                        message: "Cannot force unwrap non-optional type.",
                    }),
                }
            }
            Expr::List {
                elements,
                bracket_token,
            } => {
                let expected_owned = expected.clone();
                let expected_inner = match expected_owned {
                    Type::List(inner) => Some(inner.as_ref().clone()),
                    Type::Optional(inner) => match inner.as_ref() {
                        Type::List(list_inner) => Some(list_inner.as_ref().clone()),
                        _ => None,
                    },
                    _ => None,
                };

                if elements.is_empty() {
                    let inner = expected_inner.ok_or(TypeCheckerError::CannotInferType {
                        span: bracket_token.span,
                        uninferred_generics: vec![],
                    })?;
                    return Ok(TypedExpr {
                        ty: Type::List(Box::new(inner)),
                        kind: ExprKind::List { elements: vec![] },
                        span: expr.span(),
                    });
                }

                let mut typed_elements = Vec::with_capacity(elements.len());
                let inner_ty = expected_inner.unwrap_or_else(|| self.infer_ctx.new_type_var());

                for element in elements.iter() {
                    let typed = self
                        .coerce_expression(element, &inner_ty)
                        .recover(&mut self.errors, TypedExpr::new_blank(element.span()));
                    typed_elements.push(typed);
                }

                let inferred_inner = self.infer_ctx.substitute(&inner_ty);
                if !inferred_inner.is_concrete() {
                    return Err(TypeCheckerError::CannotInferType {
                        span: bracket_token.span,
                        uninferred_generics: vec![],
                    });
                }

                Ok(TypedExpr {
                    ty: Type::List(Box::new(inferred_inner)),
                    kind: ExprKind::List {
                        elements: typed_elements,
                    },
                    span: expr.span(),
                })
            }
            Expr::Map { .. } => {
                todo!()
            }
            Expr::GetIndex {
                safe,
                object,
                index,
            } => {
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
            Expr::SetIndex {
                safe,
                object,
                index,
                value,
            } => {
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
            Expr::Error => {
                // Already reported by parser
                Ok(TypedExpr::new_blank(expr.span()))
            }
        }
    }

    pub(crate) fn coerce_expression(
        &mut self,
        expr: &Expr<'src>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mut expr = self.check_expression(expr, expected)?;
        if self.infer_ctx.unify_types(expected, &expr.ty).is_ok() {
            let expr_ty = self.infer_ctx.substitute(&expr.ty);
            return if expr_ty.is_concrete() {
                expr.ty = expr_ty;
                Ok(expr)
            } else {
                Err(TypeCheckerError::CannotInferType {
                    span: expr.span,
                    uninferred_generics: vec![], // TODO Think how to report error
                })
            };
        }

        let expected_type = if let Type::Optional(inner) = expected {
            inner
        } else {
            expected
        };

        if let (Type::Interface(iface_name, generics), Some(name)) =
            (expected_type, expr.ty.get_name())
            && let Some(idx) = self.sys.get_vtable_idx(name, iface_name.clone())
        {
            return Ok(TypedExpr {
                ty: Type::Interface(iface_name.clone(), generics.clone()),
                span: expr.span,
                kind: ExprKind::InterfaceUpcast {
                    expr: Box::new(expr),
                    vtable_idx: idx,
                },
            });
        }

        self.infer_ctx
            .unify_types(expected, &expr.ty)
            .map_err(|msg| TypeCheckerError::ComplexTypeMismatch {
                expected: self.infer_ctx.substitute(expected),
                span: expr.span,
                message: msg,
                found: expr.ty.clone(),
            })
            .map(|_| expr)
    }
}
