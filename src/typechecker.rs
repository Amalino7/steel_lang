use crate::ast::{Expr, Literal, Stmt, Type};
use crate::token::{Token, TokenType};
use crate::typechecker::TypeCheckerError::UndefinedVariable;
use std::collections::HashMap;

#[derive(Debug)]
pub enum TypeCheckerError {
    UndefinedVariable {
        name: String,
        line: usize,
    },
    CalleeIsNotAFunction {
        found: Type,
        message: &'static str,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        line: usize,
        message: &'static str,
    },
    UndeclaredFunction {
        name: String,
        line: usize,
    },
    IncorrectArity {
        callee_name: String,
        expected: usize,
        found: usize,
    },
    InvalidReturnOutsideFunction {
        line: usize,
    },
    FunctionParameterTypeMismatch {
        expected: Type,
        found: Type,
    },
}

#[derive(Debug, PartialEq, Clone)]
enum FunctionContext {
    None,
    Function(Type),
}

struct VariableContext<'src> {
    type_: Type,
    name: &'src str,
    index: usize,
}

impl<'src> VariableContext<'src> {
    fn new(name: &'src str, type_: Type, index: usize) -> Self {
        VariableContext { type_, name, index }
    }
}

struct Scope<'src> {
    variables: HashMap<&'src str, VariableContext<'src>>,
    depth: usize,
}

pub struct TypeChecker<'src> {
    current_function: FunctionContext,
    variable_scope: Vec<Scope<'src>>,
}

impl<'src> TypeChecker<'src> {
    pub fn new() -> Self {
        TypeChecker {
            current_function: FunctionContext::None,
            variable_scope: vec![],
        }
    }
    pub fn check(&mut self, ast: &mut [Stmt<'src>]) -> Result<(), TypeCheckerError> {
        self.begin_scope();
        self.declare_global_functions(ast)?; // First pass declare all global functions.

        for stmt in ast.iter_mut() {
            self.check_stmt(stmt)?;
        }
        self.end_scope();
        Ok(())
    }
    fn begin_scope(&mut self) {
        self.variable_scope.push(Scope {
            variables: Default::default(),
            depth: self.variable_scope.len(),
        });
    }
    fn end_scope(&mut self) {
        self.variable_scope.pop();
    }

    fn declare_variable(
        &mut self,
        name: &'src str,
        type_info: Type,
    ) -> Result<(), TypeCheckerError> {
        if let Some(scope) = self.variable_scope.last_mut() {
            scope.variables.insert(
                name,
                VariableContext::new(name, type_info, scope.variables.len()),
            );
        }
        Ok(())
    }

    fn lookup_variable(&mut self, name: &str) -> Option<&VariableContext> {
        for scope in self.variable_scope.iter().rev() {
            if let Some(var) = scope.variables.get(name) {
                return Some(var);
            }
        }
        None
    }
    fn declare_global_functions(&mut self, ast: &[Stmt<'src>]) -> Result<(), TypeCheckerError> {
        for stmt in ast.iter() {
            if let Stmt::Function {
                name,
                params: _,
                type_: type_info,
                body: _,
            } = stmt
            {
                self.declare_variable(name.lexeme, type_info.clone())?;
            }
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &mut Stmt<'src>) -> Result<(), TypeCheckerError> {
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
                        line: 0, // TODO
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
                        line: 0, // TODO
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
            // TODO check if functions that must return a value are actually returning a value.
            Stmt::Return(expr) => {
                let return_type = self.infer_expression(expr)?;
                if let FunctionContext::Function(func_return_type) = &self.current_function {
                    if return_type != *func_return_type {
                        return Err(TypeCheckerError::TypeMismatch {
                            expected: func_return_type.clone(),
                            found: return_type,
                            line: 0, // TODO
                            message: "Mismatched return types. Expected a function that returns a different type than the function that is being called.",
                        });
                    }
                } else {
                    return Err(TypeCheckerError::InvalidReturnOutsideFunction {
                        line: 0, // TODO
                    });
                }
            }
        }
        Ok(())
    }
    fn infer_expression(&mut self, expr: &mut Expr) -> Result<Type, TypeCheckerError> {
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
            Expr::Literal(literal) => match literal {
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
                    return Err(UndefinedVariable {
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
                        });
                    }

                    for (i, arg) in arguments.iter_mut().enumerate() {
                        let arg_type = self.infer_expression(arg)?;
                        if arg_type != param_types[i] {
                            return Err(TypeCheckerError::FunctionParameterTypeMismatch {
                                expected: param_types[i].clone(),
                                found: arg_type,
                            });
                        }
                    }
                    *return_type
                } else {
                    return Err(TypeCheckerError::CalleeIsNotAFunction {
                        found: func_type,
                        message: "Expected a function but found something else.",
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
                    todo!()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::scanner::Scanner;

    #[test]
    fn test_type_checker() {
        let source = r#"
            let a = 5;
            let b = 10.0;
            let c = "Hello World!";
            let d = true;
            let e = false;
            let f = 5 + 10;
            let g = 5 - 10;"#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        checker
            .check(ast.as_mut_slice())
            .expect("Type checker failed.");
        println!("{:#?}", ast);
        println!(
            "{}",
            ast.iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        );
    }
    #[test]
    fn test_type_checker_function_call() {
        let source = r#"
            func add(a:string, b:string): string {
                return a + b;
            }

            let result = add("5", "10");

            if true {
                result + result;
            }

            func f1(){
                f2();
            }
            func f2(){
                f1();
            }
            "#;
        let scanner = Scanner::new(source);
        let mut parser = Parser::new(scanner);
        let mut ast = parser.parse().unwrap();
        let mut checker = TypeChecker::new();
        checker
            .check(ast.as_mut_slice())
            .expect("Type checker failed.");
        println!("{:#?}", ast);
        println!(
            "{}",
            ast.iter()
                .map(|stmt| stmt.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        );
    }
}
