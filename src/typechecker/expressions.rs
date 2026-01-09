use crate::compiler::analysis::ResolvedVar;
use crate::compiler::analysis::ResolvedVar::Global;
use crate::parser::ast::{Expr, Literal};
use crate::token::TokenType;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::error::TypeCheckerError::AssignmentToCapturedVariable;
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
        expected: Option<&Type>,
    ) -> Result<TypedExpr, TypeCheckerError> {
        match expr {
            Expr::TypeSpecialization {
                callee,
                generics: generics_provided,
            } => {
                let mut callee_typed = self.check_expression(callee, None)?;
                let generic_args = generics_provided
                    .iter()
                    .map(|t| Type::from_ast(t, &self.sys))
                    .collect::<Result<Vec<_>, _>>()?;

                match &mut callee_typed.ty {
                    Type::Function(func) => {
                        if func.type_params.len() != generic_args.len() {
                            return Err(TypeCheckerError::InvalidGenericSpecification {
                                line: callee_typed.line,
                                message: format!(
                                    "Expected {} generic arguments but got {}!",
                                    func.type_params.len(),
                                    generic_args.len(),
                                ),
                            });
                        }
                        if func.type_params.is_empty() {
                            return Err(TypeCheckerError::InvalidGenericSpecification {
                                line: callee_typed.line,
                                message: "Cannot specialize generic on function without generics!"
                                    .to_string(),
                            });
                        }
                        let map = generics_to_map(&func.type_params, &generic_args, None);
                        let new_ty = TypeSystem::generic_to_concrete(callee_typed.ty, &map);
                        callee_typed.ty = new_ty;
                        Ok(callee_typed)
                    }
                    Type::Metatype(type_name, generics) => {
                        if !generics.is_empty() {
                            return Err(TypeCheckerError::InvalidGenericSpecification {
                                line: callee_typed.line,
                                message: "Cannot specialize generic more than once!".to_string(),
                            });
                        }
                        let actual_generic_count = self.sys.get_generics_count(type_name);
                        if generic_args.len() != actual_generic_count {
                            Err(TypeCheckerError::InvalidGenericSpecification {
                                line: callee_typed.line,
                                message: format!(
                                    "Expected {} but got {} generic arguments!",
                                    actual_generic_count,
                                    generic_args.len(),
                                ),
                            })
                        } else {
                            Rc::get_mut(generics).unwrap().extend(generic_args); // Shouldn't fail
                            Ok(callee_typed)
                        }
                    }
                    _ => Err(TypeCheckerError::InvalidGenericSpecification {
                        line: callee_typed.line,
                        message: "Cannot specialize non-generic type!".to_string(),
                    }),
                }
            }
            Expr::Is {
                expression,
                type_name,
            } => {
                let target = self.check_expression(expression, None)?;
                let Type::Enum(enum_name, _) = &target.ty else {
                    return Err(TypeCheckerError::InvalidIsUsage {
                        line: type_name.line,
                        message: "Is can only be used on enum types.",
                    });
                };
                let enum_def = self
                    .sys
                    .get_enum(enum_name.as_ref())
                    .expect("Invalid enum Type return!");

                if !enum_def.variants.contains_key(type_name.lexeme) {
                    return Err(TypeCheckerError::InvalidIsUsage {
                        line: type_name.line,
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
                    line: type_name.line,
                })
            }
            Expr::Tuple { elements } => {
                let mut typed_elements = Vec::with_capacity(elements.len());
                let mut type_vec = vec![];

                let expected_types = if let Some(Type::Tuple(t)) = expected {
                    Some(&t.types)
                } else {
                    None
                };

                for (idx, element) in elements.iter().enumerate() {
                    let target_inner = expected_types.and_then(|v| v.get(idx));
                    let el = self.check_expression(element, target_inner)?;
                    type_vec.push(el.ty.clone());
                    typed_elements.push(el);
                }

                let ty = Type::Tuple(Rc::new(TupleType { types: type_vec }));
                Ok(TypedExpr {
                    ty,
                    kind: ExprKind::Tuple {
                        elements: typed_elements,
                    },
                    line: elements[0].get_line(),
                })
            }
            Expr::Unary {
                operator,
                expression,
            } if operator.token_type == TokenType::Try => {
                // TODO add suggested type
                let mut typed_expr = self.check_expression(expression, None)?;
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

                    if let FunctionContext::Function(func_return_type) =
                        self.current_function.clone()
                    {
                        let provided_err =
                            Type::Enum(name.clone(), vec![Type::Never, err_type].into());
                        let ok = self.infer_ctx.unify_types(&func_return_type, &provided_err);
                        if ok.is_err() {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: func_return_type,
                                found: provided_err,
                                line: operator.line,
                                message: "The Result type propagate by try must be compatible with the function return type.",
                            });
                        }
                    } else {
                        return Err(TypeCheckerError::InvalidReturnOutsideFunction {
                            line: operator.line,
                        });
                    }

                    Ok(TypedExpr {
                        ty: ok_type,
                        kind: ExprKind::Try {
                            operand: Box::new(typed_expr),
                        },
                        line: operator.line,
                    })
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Enum("Result".into(), vec![Type::Any, Type::Any].into()),
                        found: typed_expr.ty,
                        line: operator.line,
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
                        line: name.line,
                    })
                } else if let Some(type_name) = self.sys.get_owned_type_name(name.lexeme) {
                    Ok(TypedExpr {
                        ty: Type::Metatype(type_name.clone(), vec![].into()),
                        kind: ExprKind::GetVar(Global(0), type_name),
                        line: name.line,
                    })
                } else {
                    Err(TypeCheckerError::UndefinedVariable {
                        name: name.lexeme.to_string(),
                        line: name.line,
                    })
                }
            }
            Expr::Grouping { expression } => self.check_expression(expression, expected),
            Expr::Literal { literal, line } => {
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
                    line: *line,
                })
            }
            Expr::Assignment { identifier, value } => {
                let var_lookup = self.scopes.lookup(identifier.lexeme);

                if let Some((ctx, resolved)) = var_lookup {
                    if let ResolvedVar::Closure(_) = &resolved {
                        return Err(AssignmentToCapturedVariable {
                            name: ctx.name.to_string(),
                            line: identifier.line,
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
                        line: identifier.line,
                    })
                } else {
                    Err(TypeCheckerError::UndefinedVariable {
                        name: identifier.lexeme.to_string(),
                        line: identifier.line,
                    })
                }
            }
            Expr::Logical {
                operator,
                left,
                right,
            } if operator.token_type == TokenType::QuestionQuestion => {
                let wrapped_expected = expected.map(|t| t.clone().wrap_in_optional());

                let left_typed = self.check_expression(left, wrapped_expected.as_ref())?;
                let right_typed = self.check_expression(right, expected)?;
                let left_inner = match &left_typed.ty {
                    Type::Optional(inner) => inner.as_ref(),
                    _ => {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Optional(Box::new(Type::Any)),
                            found: left_typed.ty,
                            line: operator.line,
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
                        line: operator.line,
                    })
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: left_inner.clone(),
                        found: right_typed.ty,
                        line: operator.line,
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
                let mut callee_typed = self.check_expression(callee, None)?;
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
                    let bound_args = self.bind_arguments(
                        name,
                        &abstracted,
                        arguments,
                        false,
                        callee.get_line(),
                    )?;

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
                        line: callee.get_line(),
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
                        callee.get_line(),
                    )?;

                    *value = Box::from(init_expr);
                    let result = Type::Enum(name.clone(), Rc::new(concrete_generics));
                    callee_typed.ty = self.infer_ctx.substitute(&result);

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
                    .unwrap_optional_safe(safe, callee_typed.line)?;

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
                            callee.to_string().as_ref(),
                            &func.params,
                            arguments,
                            func.is_vararg,
                            callee_typed.line,
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
                                line: callee.get_line(),
                                uninferred_generics: uninferred,
                            });
                        }
                        let ret_type = self.infer_ctx.substitute(&ret_type);

                        Ok(TypedExpr {
                            ty: ret_type,
                            line: callee_typed.line,
                            kind: ExprKind::Call {
                                callee: Box::new(callee_typed),
                                arguments: bound_args,
                                safe,
                            },
                        })
                    }
                    _ => Err(TypeCheckerError::CalleeIsNotCallable {
                        found: callee_typed.ty,
                        line: callee_typed.line,
                    }),
                }
            }
            Expr::Get {
                object,
                field,
                safe,
            } => {
                //TODO Ignores safe static access probably should be a warning
                let object_typed = self.check_expression(object, None)?;
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
                        kind: ExprKind::SetField {
                            object: Box::new(typed_obj),
                            index,
                            value: Box::new(typed_value),
                            safe: *safe,
                        },
                        line: field.line,
                    })
                },
            ),

            Expr::ForceUnwrap { expression, line } => {
                let expected_opt = expected.map(|t| Type::Optional(Box::new(t.clone())));

                let expr_typed = self.check_expression(expression, expected_opt.as_ref())?;
                match expr_typed.ty.clone() {
                    Type::Optional(inner) => Ok(TypedExpr {
                        ty: *inner,
                        kind: ExprKind::Unary {
                            operator: UnaryOp::Unwrap,
                            operand: Box::new(expr_typed),
                        },
                        line: *line,
                    }),
                    _ => Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Optional(Box::new(Type::Any)),
                        found: expr_typed.ty,
                        line: *line,
                        message: "Cannot force unwrap non-optional type.",
                    }),
                }
            }
        }
    }

    pub(crate) fn coerce_expression(
        &mut self,
        expr: &Expr<'src>,
        expected: &Type,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mut expr = self.check_expression(expr, Some(expected))?;
        if self.infer_ctx.unify_types(expected, &expr.ty).is_ok() {
            let expr_ty = self.infer_ctx.substitute(&expr.ty);
            return if expr_ty.is_concrete() {
                expr.ty = expr_ty;
                Ok(expr)
            } else {
                Err(TypeCheckerError::CannotInferType {
                    line: expr.line,
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
                line: expr.line,
                kind: ExprKind::InterfaceUpcast {
                    expr: Box::new(expr),
                    vtable_idx: idx,
                },
            });
        }

        self.infer_ctx
            .unify_types(expected, &expr.ty)
            .map_err(|msg| TypeCheckerError::ComplexTypeMismatch {
                expected: expected.clone(),
                line: expr.line,
                message: msg,
                found: expr.ty.clone(),
            })
            .map(|_| expr)
    }
}
