use crate::parser::ast::CallArg;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::TypedExpr;
use crate::typechecker::types::Type;
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn bind_arguments(
        &mut self,
        callee_name: &str,
        params: &[(String, Type)],
        args: &[CallArg<'src>],
        is_vararg: bool,
        call_line: u32,
    ) -> Result<Vec<TypedExpr>, TypeCheckerError> {
        let fixed_len = params.len();
        let mut fixed: Vec<Option<TypedExpr>> = vec![None; fixed_len];
        let mut used = vec![false; fixed_len];
        let mut extras = Vec::new(); // Used for varargs
        let mut pos_cursor = 0;
        let mut seen_named = false;

        for CallArg { label, expr } in args {
            let line = expr.get_line();
            match label {
                Some(name) => {
                    seen_named = true;

                    let idx = self
                        .sys
                        .resolve_named_arg(callee_name, params, name.lexeme, line)?;

                    if used[idx] {
                        return Err(TypeCheckerError::DuplicateArgument {
                            name: params[idx].0.clone(),
                            line,
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
                            callee: callee_name.to_string(),
                            message: "positional arguments cannot appear after named arguments",
                            line,
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
                            callee: callee_name.to_string(),
                            expected: fixed_len,
                            found: fixed_len + extras.len() + 1,
                            line,
                        });
                    }
                }
            }
        }

        let mut result = Vec::with_capacity(fixed_len + extras.len());

        for (i, opt) in fixed.into_iter().enumerate() {
            result.push(opt.ok_or_else(|| TypeCheckerError::MissingArgument {
                param_name: params[i].0.clone(),
                callee: callee_name.to_string(),
                line: call_line,
            })?);
        }

        result.extend(extras);
        Ok(result)
    }
}
