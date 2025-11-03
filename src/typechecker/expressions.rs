use crate::parser::ast::{Expr, Literal, Type};
use crate::token::{Token, TokenType};
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::TypeChecker;

impl TypeChecker<'_> {
    pub(crate) fn infer_expression(&mut self, expr: &mut Expr) -> Result<Type, TypeCheckerError> {
        let inferred_type: Type = match expr {
            Expr::Unary {
                operator,
                expression,
            } => {
                let type_ = self.infer_expression(expression)?;
                if operator.token_type == TokenType::Bang {
                    if type_ == Type::Boolean {
                        Type::Boolean
                    } else {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Boolean,
                            found: type_,
                            line: operator.line,
                            message: "Expected boolean but found something else.",
                        });
                    }
                } else if operator.token_type == TokenType::Minus {
                    if type_ == Type::Number {
                        Type::Number
                    } else {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: Type::Number,
                            found: type_,
                            line: operator.line,
                            message: "Expected number but found something else.",
                        });
                    }
                } else {
                    unreachable!("Ast should be checked for invalid operators before this point.")
                }
            }
            Expr::Binary {
                operator,
                left,
                right,
            } => self.check_binary_expression(operator, left, right)?,

            Expr::Variable { name, scope, index } => {
                let var = self.lookup_variable(name.lexeme);
                if let Some(var) = var {
                    let type_info = var.type_.clone();
                    let var_index = var.index;
                    let _ = index.insert(var_index);
                    let scope_depth = self.variable_scope.len();
                    let _ = scope.insert(scope_depth);
                    type_info
                } else {
                    return Err(TypeCheckerError::UndefinedVariable {
                        name: name.lexeme.to_string(),
                        line: name.line,
                    });
                }
            }
            Expr::Grouping { expression } => self.infer_expression(expression)?,
            Expr::Literal { literal, line: _ } => match literal {
                Literal::Number(_) => Type::Number,
                Literal::String(_) => Type::String,
                Literal::Boolean(_) => Type::Boolean,
                Literal::Void => Type::Void,
            },
            Expr::Assignment { identifier, value } => {
                let value_type = self.infer_expression(value)?;
                let var_type = self.lookup_variable(identifier.lexeme);
                if let Some(var) = var_type {
                    if value_type != var.type_ {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: var.type_.clone(),
                            found: value_type,
                            line: identifier.line,
                            message: "Expected the same type but found something else.",
                        });
                    } else {
                        value_type
                    }
                } else {
                    return Err(TypeCheckerError::UndefinedVariable {
                        name: identifier.lexeme.to_string(),
                        line: identifier.line,
                    });
                }
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left_type = self.infer_expression(left)?;
                let right_type = self.infer_expression(right)?;
                match operator.token_type {
                    TokenType::And | TokenType::Or => {
                        if left_type == Type::Boolean && right_type == Type::Boolean {
                            Type::Boolean
                        } else if left_type != Type::Boolean {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: Type::Boolean,
                                found: left_type,
                                line: operator.line,
                                message: "Expected boolean but found something else.",
                            });
                        } else if right_type != Type::Boolean {
                            return Err(TypeCheckerError::TypeMismatch {
                                expected: Type::Boolean,
                                found: right_type,
                                line: operator.line,
                                message: "Expected boolean but found something else.",
                            });
                        } else {
                            unreachable!()
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Expr::Call { callee, arguments } => {
                let func_type = self.infer_expression(callee)?;
                if let Type::Function {
                    param_types,
                    return_type,
                } = func_type
                {
                    if param_types.len() != arguments.len() {
                        return Err(TypeCheckerError::IncorrectArity {
                            callee_name: callee.to_string(),
                            expected: param_types.len(),
                            found: arguments.len(),
                            line: callee.get_line(),
                        });
                    }

                    for (i, arg) in arguments.iter_mut().enumerate() {
                        let arg_type = self.infer_expression(arg)?;
                        if arg_type != param_types[i] {
                            return Err(TypeCheckerError::FunctionParameterTypeMismatch {
                                expected: param_types[i].clone(),
                                found: arg_type,
                                param_index: i,
                                line: arg.get_line(),
                            });
                        }
                    }
                    *return_type
                } else {
                    return Err(TypeCheckerError::CalleeIsNotAFunction {
                        found: func_type,
                        line: callee.get_line(),
                    });
                }
            }
        };
        Ok(inferred_type)
    }

    fn check_binary_expression(
        &mut self,
        operator: &mut Token,
        left: &mut Box<Expr>,
        right: &mut Box<Expr>,
    ) -> Result<Type, TypeCheckerError> {
        let left_type = self.infer_expression(left)?;
        let right_type = self.infer_expression(right)?;
        match operator.token_type {
            TokenType::Minus | TokenType::Slash | TokenType::Star => {
                if left_type == Type::Number && right_type == Type::Number {
                    Ok(Type::Number)
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Number,
                        found: left_type,
                        line: operator.line,
                        message: "Expected number but found something else.",
                    })
                }
            }
            TokenType::Plus => {
                if left_type == Type::Number && right_type == Type::Number {
                    Ok(Type::Number)
                } else if left_type == Type::String && right_type == Type::String {
                    Ok(Type::String)
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: left_type,
                        found: right_type,
                        line: operator.line,
                        message: "Operands to '+' must be both numbers or both strings.",
                    })
                }
            }
            TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => {
                if left_type == Type::Number && right_type == Type::Number {
                    Ok(Type::Boolean)
                } else if left_type == Type::String && right_type == Type::String {
                    Ok(Type::Boolean)
                } else {
                    return Err(TypeCheckerError::TypeMismatch {
                        expected: Type::Number,
                        found: left_type,
                        line: operator.line,
                        message: "Expected number but found something else.",
                    });
                }
            }
            TokenType::EqualEqual | TokenType::BangEqual => {
                if left_type == right_type {
                    Ok(Type::Boolean)
                } else {
                    Err(TypeCheckerError::TypeMismatch {
                        expected: left_type,
                        found: right_type,
                        line: operator.line,
                        message: "Expected the same type but found something else.",
                    })
                }
            }
            _ => {
                unreachable!("Ast should be checked for invalid operators before this point.")
            }
        }
    }
}
