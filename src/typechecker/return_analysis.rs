use crate::parser::ast::Stmt;
use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::Type;
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_returns(
        &mut self,
        stmt: &mut [Stmt<'src>],
        errors: &mut Vec<TypeCheckerError>,
    ) {
        for stmt in stmt.iter_mut() {
            let res = self.check_stmt_returns(stmt);
            if let Err(e) = res {
                errors.push(e);
            }
        }
    }
    fn check_stmt_returns(&mut self, stmt: &mut Stmt<'src>) -> Result<(), TypeCheckerError> {
        match stmt {
            Stmt::Expression(_) => Ok(()),
            Stmt::Let { .. } => Ok(()),
            Stmt::Block { body, .. } => {
                for stmt in body {
                    self.check_stmt_returns(stmt)?;
                }
                Ok(())
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                if let Some(else_branch) = else_branch {
                    self.check_stmt_returns(else_branch)?;
                }
                self.check_stmt_returns(then_branch)
            }
            Stmt::While { body, .. } => self.check_stmt_returns(body),
            Stmt::Function {
                name,
                params,
                body,
                type_,
            } => {
                let mut returns = false;
                let return_type = if let Some(Type::Function(func)) = Type::from_ast(type_) {
                    func.return_type.clone()
                } else {
                    unreachable!()
                };

                for stmt in body {
                    if returns {
                        return Err(TypeCheckerError::UnreachableCode {
                            line: stmt.get_line(),
                        });
                    }

                    self.check_stmt_returns(stmt)?;
                    if self.stmt_returns(stmt) {
                        returns = true;
                    }
                }

                if !returns && return_type != Type::Void {
                    Err(TypeCheckerError::MissingReturnStatement {
                        line: stmt.get_line(),
                    })
                } else {
                    Ok(())
                }
            }
            Stmt::Return(_) => Ok(()),
        }
    }
    fn stmt_returns(&mut self, stmt: &mut Stmt<'src>) -> bool {
        match stmt {
            Stmt::Expression(_) => false,
            Stmt::Let { .. } => false,
            Stmt::Block { body, .. } => body.iter_mut().any(|e| self.stmt_returns(e)),
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                if let Some(else_branch) = else_branch {
                    self.stmt_returns(else_branch) && self.stmt_returns(then_branch)
                } else {
                    false
                }
            }
            Stmt::While { .. } => false,
            Stmt::Function { .. } => false,
            Stmt::Return(_) => true,
        }
    }
}
