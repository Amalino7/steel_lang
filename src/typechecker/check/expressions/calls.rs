use crate::parser::ast::{CallArg, Expr};
use crate::scanner::Span;
use crate::typechecker::core::ast::{ExprKind, TypedExpr};
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::core::types::{TupleType, Type};
use crate::typechecker::system::generics_to_map;
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashMap;
use std::rc::Rc;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_call(
        &mut self,
        expr: &Expr<'src>,
        callee: &Expr<'src>,
        arguments: &Vec<CallArg<'src>>,
        safe: &bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
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
            let abstract_type_args = struct_def
                .generic_params
                .iter()
                .map(|name| map.get(name).unwrap())
                .collect::<Vec<_>>();
            let abstracted_field = struct_def
                .ordered_fields
                .iter()
                .map(|(s, ty)| (s.clone(), ty.clone().generic_to_concrete(&map)))
                .collect::<Vec<_>>();

            let owned_name = struct_def.name.clone();
            let bound_args =
                self.bind_arguments(callee.span(), &abstracted_field, arguments, false)?;

            let type_args = abstract_type_args
                .iter()
                .map(|t| self.infer_ctx.substitute(t))
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
            let (variant_name, variant_type) = &enum_def.ordered_variants[*variant_idx as usize];
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

                let Type::Function(func) = Type::Function(func.clone()).generic_to_concrete(&map)
                else {
                    panic!("Should have errored earlier");
                };

                let bound_args =
                    self.bind_arguments(callee.span(), &func.params, arguments, func.is_vararg)?;
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

    fn handle_enum_call(
        &mut self,
        _variant_name: Symbol,
        variant_type: &Type,
        map: &HashMap<Symbol, Type>,
        inferred_args: &Vec<CallArg<'src>>,
        span: Span,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let bind_and_process = |this: &mut Self,
                                raw_params: Vec<(String, Type)>|
         -> Result<Vec<TypedExpr>, TypeCheckerError> {
            let concrete_params: Vec<(String, Type)> = raw_params
                .into_iter()
                .map(|(name, ty)| (name, ty.generic_to_concrete(map)))
                .collect();

            this.bind_arguments(span, &concrete_params, inferred_args, false)
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
                    .map(|t| Type::GenericParam(t.clone()).generic_to_concrete(map))
                    .collect::<Vec<_>>();

                let params = struct_def.ordered_fields.clone();

                let bound_args = bind_and_process(self, params)?;

                TypedExpr {
                    ty: Type::Struct(struct_name.clone(), Rc::new(concrete_struct_generics)),
                    kind: ExprKind::StructInit {
                        name: Box::from(struct_name.to_string()),
                        args: bound_args,
                    },
                    span,
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
                    .map(|t| t.clone().generic_to_concrete(map))
                    .collect::<Vec<_>>();

                TypedExpr {
                    ty: Type::Tuple(Rc::new(TupleType {
                        types: concrete_tuple_types,
                    })),
                    kind: ExprKind::Tuple {
                        elements: bound_args,
                    },
                    span,
                }
            }

            // Case: MyEnum.Value(5)
            _ => {
                let params = vec![("value".to_string(), variant_type.clone())];

                let mut bound_args = bind_and_process(self, params)?;
                bound_args.pop().ok_or(TypeCheckerError::MissingArgument {
                    param_name: "value".into(),
                    span,
                    callee_origin: None, // TODO: track function definition span
                })?
            }
        };

        Ok(val_expr)
    }

    fn bind_arguments(
        &mut self,
        callee: Span,
        params: &[(String, Type)],
        args: &[CallArg<'src>],
        is_vararg: bool,
    ) -> Result<Vec<TypedExpr>, TypeCheckerError> {
        let fixed_len = params.len();
        let mut fixed: Vec<Option<TypedExpr>> = (0..fixed_len).map(|_| None).collect();
        let mut used = vec![false; fixed_len];
        let mut extras = Vec::new(); // Used for varargs
        let mut pos_cursor = 0;
        let mut seen_named = false;

        for CallArg { label, expr } in args {
            let span = expr.span();
            match label {
                Some(name) => {
                    seen_named = true;

                    let idx = resolve_named_arg(params, name.lexeme, name.span)?;

                    if used[idx] {
                        return Err(TypeCheckerError::DuplicateArgument {
                            name: params[idx].0.clone(),
                            span: name.span.merge(span),
                        });
                    }

                    let expected = &params[idx].1;
                    let coerced = self.coerce_expression(expr, expected)?;
                    fixed[idx] = Some(coerced);
                    used[idx] = true;
                }

                None => {
                    if seen_named {
                        return Err(TypeCheckerError::PositionalArgumentAfterNamed {
                            message: "positional arguments cannot appear after named arguments",
                            span,
                        });
                    }

                    while pos_cursor < fixed_len && used[pos_cursor] {
                        pos_cursor += 1;
                    }

                    if pos_cursor < fixed_len {
                        let expected = &params[pos_cursor].1;
                        let coerced = self.coerce_expression(expr, expected)?;
                        fixed[pos_cursor] = Some(coerced);
                        used[pos_cursor] = true;
                        pos_cursor += 1;
                    } else if is_vararg {
                        extras.push(self.coerce_expression(expr, &params[pos_cursor - 1].1)?);
                    } else {
                        return Err(TypeCheckerError::TooManyArguments {
                            expected: fixed_len,
                            found: fixed_len + extras.len() + 1,
                            span,
                            callee,
                        });
                    }
                }
            }
        }

        let mut result = Vec::with_capacity(fixed_len + extras.len());

        for (i, opt) in fixed.into_iter().enumerate() {
            result.push(opt.ok_or_else(|| {
                TypeCheckerError::MissingArgument {
                    param_name: params[i].0.clone(),
                    span: args
                        .last()
                        .map(|arg| arg.expr.span().merge(callee))
                        .unwrap_or_else(|| callee),
                    callee_origin: Some(callee), // TODO: pass actual function definition span
                }
            })?);
        }

        result.extend(extras);
        Ok(result)
    }
}
fn resolve_named_arg(
    params: &[(String, Type)],
    name: &str,
    span: Span,
) -> Result<usize, TypeCheckerError> {
    params
        .iter()
        .position(|(pname, _)| pname == name)
        .ok_or_else(|| TypeCheckerError::UndefinedParameter {
            param_name: name.to_string(),
            span,
            callee_origin: None, // TODO: pass actual function definition span
        })
}
