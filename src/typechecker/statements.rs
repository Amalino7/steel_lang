use crate::parser::ast::{Stmt, Type};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::{FunctionContext, TypeChecker};

impl<'src> TypeChecker<'src> {
    pub(crate) fn declare_global_functions(
        &mut self,
        ast: &[Stmt<'src>],
        errors: &mut Vec<TypeCheckerError>,
    ) {
        for stmt in ast.iter() {
            if let Stmt::Function {
                name,
                params: _,
                type_: type_info,
                body: _,
            } = stmt
            {
                let res = self.declare_variable(name.lexeme, type_info.clone());
                if let Err(e) = res {
                    errors.push(e);
                }
            }
        }
    }

    pub(crate) fn check_stmt(&mut self, stmt: &mut Stmt<'src>) -> Result<(), TypeCheckerError> {
        match stmt {
            Stmt::Expression(expr) => {
                self.infer_expression(expr)?;
            }
            Stmt::Let {
                identifier,
                value,
                type_info,
            } => {
                let value_type = self.infer_expression(value)?;
                if *type_info == Type::Unknown {
                    *type_info = value_type.clone();
                    self.declare_variable(identifier.lexeme, value_type)?;
                } else if *type_info == value_type {
                    self.declare_variable(identifier.lexeme, value_type)?;
                } else {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: type_info.clone(),
                        found: value_type,
                        line: identifier.line,
                        message: "Expected the same type but found something else.",
                    });
                }
            }
            Stmt::Block(statements) => {
                self.begin_scope();
                for stmt in statements {
                    self.check_stmt(stmt)?;
                }
                self.end_scope();
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_type = self.infer_expression(condition)?;
                self.check_stmt(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.check_stmt(else_branch)?;
                }
                if cond_type != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_type,
                        line: condition.get_line(),
                        message: "If condition must be a boolean.",
                    });
                }
            }
            Stmt::While { condition, body } => {
                let cond_type = self.infer_expression(condition)?;
                self.check_stmt(body)?;
                if cond_type != Type::Boolean {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Boolean,
                        found: cond_type,
                        line: condition.get_line(),
                        message: "If condition must be a boolean.",
                    });
                }
            }
            Stmt::Function {
                name,
                params,
                body,
                type_,
            } => {
                self.declare_variable(name.lexeme, type_.clone())?;

                let enclosing_function_context = self.current_function.clone();
                if let Type::Function {
                    param_types,
                    return_type,
                } = type_
                {
                    self.current_function = FunctionContext::Function(*return_type.clone());

                    self.begin_scope();

                    // Declare parameters.
                    for (i, param) in params.iter().enumerate() {
                        self.declare_variable(param.lexeme, param_types[i].clone())?;
                    }
                    for stmt in body {
                        self.check_stmt(stmt)?;
                    }

                    self.end_scope();
                    self.current_function = enclosing_function_context;
                } else {
                    unreachable!()
                }
            }
            Stmt::Return(expr) => {
                let return_type = self.infer_expression(expr)?;
                if let FunctionContext::Function(func_return_type) = &self.current_function {
                    if return_type != *func_return_type {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: func_return_type.clone(),
                            found: return_type,
                            line: expr.get_line(),
                            message: "Mismatched return types. Expected a function that returns a different type than the function that is being called.",
                        });
                    }
                } else {
                    return Err(TypeCheckerError::InvalidReturnOutsideFunction {
                        line: expr.get_line(),
                    });
                }
            }
        }
        Ok(())
    }
}
