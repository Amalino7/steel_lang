use crate::parser::ast::{CallArg, Expr};
use crate::scanner::Span;
use crate::typechecker::core::ast::{ExprKind, TypedExpr};
use crate::typechecker::core::error::TypeCheckerError;
use crate::typechecker::core::types::Type;
use crate::typechecker::system::generics_to_map;
use crate::typechecker::{Symbol, TypeChecker};
use std::collections::HashMap;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_call(
        &mut self,
        expr: &Expr<'src>,
        callee: &Expr<'src>,
        arguments: &Vec<CallArg<'src>>,
        safe: &bool,
    ) -> Result<TypedExpr, TypeCheckerError> {
        let mut callee_typed = self.check_expression(callee, &Type::Unknown)?;

        let safe = if let ExprKind::MethodGet { safe, .. } = callee_typed.kind {
            safe
        } else if let ExprKind::InterfaceMethodGet { safe, .. } = callee_typed.kind {
            safe
        } else {
            *safe
        };

        let (lookup_type, safe) =
            callee_typed
                .ty
                .unwrap_optional_safe(safe, callee_typed.span, &mut self.warnings);

        // Handle Struct constructor
        if let Type::Metatype(name, generics) = &lookup_type
            && let Some(struct_def) = self.sys.get_struct(name)
        {
            let constructor = struct_def.get_constructor(generics, &mut self.infer_ctx);

            let owned_name = struct_def.name.clone();
            let bound_args =
                self.bind_arguments(callee.span(), &constructor.resolved_args, arguments, false)?;
            let final_type = self.infer_ctx.substitute(&constructor.constructed_type);

            return Ok(TypedExpr {
                ty: final_type,
                kind: ExprKind::StructInit {
                    name: Box::from(owned_name.to_string()),
                    args: bound_args,
                },
                span: expr.span(),
            });
        }

        if let Type::Metatype(name, generics) = &lookup_type
            && let Some(enum_def) = self.sys.get_enum(name)
            && let ExprKind::EnumInit { value, .. } = &mut callee_typed.kind
        {
            let constructor = enum_def
                .get_constructor(generics.clone(), value.ty.clone(), &self.sys)
                .unwrap();

            let span = callee.span();
            let mut bound_args =
                self.bind_arguments(span, &constructor.resolved_args, arguments, false)?;
            let final_type = self.infer_ctx.substitute(&constructor.constructed_type);
            let init_expr = match &value.ty {
                Type::Tuple(_) => TypedExpr {
                    ty: value.ty.clone(),
                    kind: ExprKind::Tuple {
                        elements: bound_args,
                    },
                    span,
                },
                Type::Struct(struct_name, ..) => TypedExpr {
                    kind: ExprKind::StructInit {
                        name: struct_name.to_string().into(),
                        args: bound_args,
                    },
                    ty: value.ty.clone(),
                    span,
                },
                _ => bound_args.pop().unwrap(),
            };
            callee_typed.ty = final_type;
            callee_typed.span = expr.span();
            *value = Box::from(init_expr);
            return Ok(callee_typed);
        }

        // Check for Normal Function Call
        if let Type::Function(func) = &lookup_type {
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
            let ret_type = self.infer_ctx.substitute(&ret_type);
            return Ok(TypedExpr {
                ty: ret_type,
                span: expr.span(),
                kind: ExprKind::Call {
                    callee: Box::new(callee_typed),
                    arguments: bound_args,
                    safe,
                },
            });
        };

        Err(TypeCheckerError::CalleeIsNotCallable {
            found: callee_typed.ty,
            span: callee_typed.span,
        })
    }

    fn bind_arguments(
        &mut self,
        callee: Span,
        params: &[(Symbol, Type)],
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
                            name: params[idx].0.to_string(),
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
                    param_name: params[i].0.to_string(),
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
    params: &[(Symbol, Type)],
    name: &str,
    span: Span,
) -> Result<usize, TypeCheckerError> {
    params
        .iter()
        .position(|(pname, _)| pname.as_ref() == name)
        .ok_or_else(|| TypeCheckerError::UndefinedParameter {
            param_name: name.to_string(),
            span,
            callee_origin: None, // TODO: pass actual function definition span
        })
}
