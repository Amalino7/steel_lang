use crate::compiler::analysis::ResolvedVar;
use crate::compiler::analysis::ResolvedVar::Global;
use crate::parser::ast::{Expr, Literal};
use crate::token::{Token, TokenType};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::error::TypeCheckerError::AssignmentToCapturedVariable;
use crate::typechecker::type_ast::{ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::type_system::{generics_to_map, TySys, TypeSystem};
use crate::typechecker::types::Type::GenericParam;
use crate::typechecker::types::{GenericArgs, StructType, TupleType, Type};
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
                        // let ok = TypeSystem::unify_types(
                        //     &func_return_type,
                        //     &mut HashMap::new(),
                        //     &provided_err,
                        // );
                        // if ok.is_err() {
                        //     return Err(TypeCheckerError::TypeMismatch {
                        //         expected: func_return_type,
                        //         found: provided_err,
                        //         line: operator.line,
                        //         message: "The Result type propagate by try must be compatible with the function return type.",
                        //     });
                        // }
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
            } => self.check_binary_expression(operator, left, right, expected),

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

                    let typed_value = self.check_expression(value, Some(&expected))?;
                    let coerced_value = self.sys.verify_assignment(
                        &mut self.infer_ctx,
                        &expected,
                        typed_value,
                        identifier.line,
                    )?;

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
                // Handle args
                let mut inferred_args = Vec::with_capacity(arguments.len());

                for arg in arguments {
                    let typed_val = self.check_expression(&arg.expr, None)?;

                    let label = arg.label.as_ref().map(|t| t.lexeme);
                    let line = arg
                        .label
                        .as_ref()
                        .map(|t| t.line)
                        .unwrap_or(callee.get_line());

                    inferred_args.push((label, typed_val, line));
                }
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

                    let res = self.infer_ctx.unify_types(
                        expected.unwrap_or(&Type::Unknown),
                        &Type::Struct(
                            name.clone(),
                            struct_def
                                .generic_params
                                .iter()
                                .map(|t| TySys::generic_to_concrete(GenericParam(t.clone()), &map))
                                .collect::<Vec<_>>()
                                .into(),
                        ),
                    );
                    res.expect("Struct constructor should be valid");

                    let owned_name = struct_def.name.clone();
                    let bound_args = self.sys.bind_arguments(
                        name,
                        &abstracted,
                        inferred_args,
                        &mut self.infer_ctx,
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
                    todo!("Handle enum init");
                    // let (variant_name, ty) = &enum_def.ordered_variants[*variant_idx as usize];
                    // // let (expr, mut map) = self.handle_enum_call(
                    // //     variant_name,
                    // //     ty,
                    // //     &enum_def.generic_params,
                    // //     generics,
                    // //     &mut self.infer_ctx,
                    // //     inferred_args,
                    // //     callee.get_line(),
                    // // )?;
                    // // let res_ty = Type::Enum(
                    // //     name.clone(),
                    // //     enum_def
                    // //         .generic_params
                    // //         .iter()
                    // //         .map(|n| Type::GenericParam(n.clone()))
                    // //         .collect::<Vec<_>>()
                    // //         .into(),
                    // // );
                    // // let err = TypeSystem::unify_types(
                    // //     expected.unwrap_or(&Type::Unknown),
                    // //     &mut map,
                    // //     &res_ty,
                    // // );
                    // // if let Err(err) = err {
                    // //     return Err(TypeCheckerError::ComplexTypeMismatch {
                    // //         expected: expected.unwrap_or(&Type::Unknown).clone(),
                    // //         found: expr.ty.clone(),
                    // //         message: err,
                    // //         line: callee.get_line(),
                    // //     });
                    // // }
                    //
                    // *value = Box::from(expr);
                    // let issue = map.iter().any(|(_, v)| *v == Type::Unknown);
                    // if issue {
                    //     return Err(TypeCheckerError::CannotInferType {
                    //         line: callee.get_line(),
                    //         uninferred_generics: map
                    //             .iter()
                    //             .filter(|(_, v)| **v == Type::Unknown)
                    //             .map(|(name, _)| name.to_string())
                    //             .collect(),
                    //     });
                    // }
                    // // TODO handle generics
                    // callee_typed.ty = Type::Enum(
                    //     name.clone(),
                    //     enum_def
                    //         .generic_params
                    //         .iter()
                    //         .map(|s| map.get(s).unwrap().clone())
                    //         .collect::<Vec<_>>()
                    //         .into(),
                    // );
                    // return Ok(callee_typed);
                }

                let safe = if let ExprKind::MethodGet { safe, .. } = callee_typed.kind {
                    safe
                } else if let ExprKind::InterfaceMethodGet { safe, .. } = callee_typed.kind {
                    safe
                } else {
                    *safe
                };

                let lookup_type = if safe {
                    match &callee_typed.ty {
                        Type::Optional(inner) => inner.as_ref().clone(),
                        _ => {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: Type::Optional(Box::new(Type::Any)),
                                found: callee_typed.ty.clone(),
                                line: callee_typed.line,
                                message: "Cannot use safe of non-optional type. Use simply '()'",
                            });
                        }
                    }
                } else {
                    callee_typed.ty.clone()
                };
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

                        // let err = self
                        //     .sys
                        //     .get_inference_ctx_mut()
                        //     .unify_types(expected.unwrap_or(&Type::Unknown), &func.return_type);
                        // if let Err(err) = err {
                        //     return Err(TypeCheckerError::ComplexTypeMismatch {
                        //         expected: expected.unwrap_or(&Type::Unknown).clone(),
                        //         found: func.return_type.clone(),
                        //         message: err,
                        //         line: callee.get_line(),
                        //     });
                        // }

                        let bound_args = self.sys.bind_arguments(
                            callee.to_string().as_ref(),
                            &func.params,
                            inferred_args,
                            &mut self.infer_ctx,
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
            } => {
                let object_typed = self.check_expression(object, None)?;
                // TODO add target type inference
                let value = self.check_expression(value, None)?;

                let type_ = if *safe {
                    match &object_typed.ty {
                        Type::Optional(inner) => inner.as_ref().clone(),
                        _ => {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: Type::Optional(Box::new(Type::Any)),
                                found: object_typed.ty.clone(),
                                line: field.line,
                                message: "Cannot access safe property of non-optional type. Use simply '.'",
                            });
                        }
                    }
                } else {
                    object_typed.ty.clone()
                };

                if let Type::Tuple(tuple_type) = &type_ {
                    let idx = match field.lexeme.parse::<u8>() {
                        Ok(idx) => idx,
                        Err(err) => {
                            return Err(TypeCheckerError::InvalidTupleIndex {
                                tuple_type: type_,
                                index: err.to_string(),
                                line: field.line,
                            });
                        }
                    };
                    if idx >= tuple_type.types.len() as u8 {
                        return Err(TypeCheckerError::InvalidTupleIndex {
                            tuple_type: type_,
                            index: idx.to_string(),
                            line: field.line,
                        });
                    }

                    return Ok(TypedExpr {
                        ty: tuple_type.types[idx as usize].clone(),
                        kind: ExprKind::SetField {
                            object: Box::from(object_typed),
                            index: idx,
                            safe: *safe,
                            value: Box::new(self.sys.verify_assignment(
                                todo!(),
                                &tuple_type.types[idx as usize],
                                value,
                                field.line,
                            )?),
                        },
                        line: field.line,
                    });
                }

                if let Type::Struct(struct_def, generics) = type_ {
                    let struct_def = self
                        .sys
                        .get_struct(&struct_def)
                        .expect("Should have errored earlier");

                    todo!()
                    // let (field_idx, field_type) =
                    //     self.check_field_type(struct_def, field, &value, generics)?;
                    //
                    // Ok(TypedExpr {
                    //     ty: field_type,
                    //     kind: ExprKind::SetField {
                    //         safe: *safe,
                    //         object: Box::new(object_typed),
                    //         index: field_idx as u8,
                    //         value: Box::new(value),
                    //     },
                    //     line: field.line,
                    // })
                } else {
                    Err(TypeCheckerError::TypeHasNoFields {
                        found: type_,
                        line: object_typed.line,
                    })
                }
            }

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

    fn check_field_type(
        &mut self,
        struct_def: &StructType,
        field: &Token,
        field_value: &TypedExpr,
        generic_args: GenericArgs,
    ) -> Result<(usize, Type), TypeCheckerError> {
        let field_type = struct_def
            .fields
            .get(field.lexeme)
            .map(|id| (*id, struct_def.ordered_fields[*id].1.clone()));

        match field_type {
            None => Err(TypeCheckerError::UndefinedField {
                struct_name: struct_def.name.to_string(),
                field_name: field.lexeme.to_string(),
                line: field.line,
            }),
            Some((id, field_type)) => {
                let actual = TypeSystem::generic_to_concrete(
                    field_type,
                    &generics_to_map(&struct_def.generic_params, &generic_args, None),
                );
                self.sys.verify_assignment(
                    &mut self.infer_ctx,
                    &actual,
                    field_value.clone(),
                    field.line,
                )?;
                Ok((id, actual))
            }
        }
    }
}
