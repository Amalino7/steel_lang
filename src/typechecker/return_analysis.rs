use crate::typechecker::error::TypeCheckerError;
use crate::typechecker::type_ast::{StmtKind, Type, TypedStmt};
use crate::typechecker::TypeChecker;

impl<'src> TypeChecker<'src> {
    pub(crate) fn check_returns(&mut self, stmt: &[TypedStmt]) {
        for stmt in stmt {
            let res = self.check_stmt_returns(stmt);
            if let Err(e) = res {
                self.errors.push(e);
            }
        }
    }
    fn check_stmt_returns(&mut self, stmt: &TypedStmt) -> Result<(), TypeCheckerError> {
        match &stmt.kind {
            StmtKind::Expression(_) => Ok(()),
            StmtKind::Let { .. } => Ok(()),
            StmtKind::Block { body, .. } => body
                .iter()
                .try_for_each(|stmt| self.check_stmt_returns(stmt)),
            StmtKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                if let Some(else_branch) = else_branch {
                    self.check_stmt_returns(else_branch)?;
                }
                self.check_stmt_returns(then_branch)
            }
            StmtKind::While { body, .. } => self.check_stmt_returns(body),
            StmtKind::Function { body, .. } => {
                let return_type = if let Type::Function(func) = &stmt.type_info {
                    func.return_type.clone()
                } else {
                    unreachable!()
                };

                self.check_stmt_returns(body)?;
                let does_return = self.stmt_returns(body)?;

                if !does_return && return_type != Type::Void {
                    Err(TypeCheckerError::MissingReturnStatement { line: stmt.line })
                } else {
                    Ok(())
                }
            }
            StmtKind::Return(_) => Ok(()),
            StmtKind::Global { stmts, .. } => stmts
                .iter()
                .try_for_each(|stmt| self.check_stmt_returns(stmt)),
            StmtKind::StructDecl { .. } => Ok(()),
            StmtKind::Impl { methods, .. } => methods
                .iter()
                .try_for_each(|method| self.check_stmt_returns(method)),
        }
    }
    fn stmt_returns(&mut self, stmt: &TypedStmt) -> Result<bool, TypeCheckerError> {
        Ok(match &stmt.kind {
            StmtKind::Expression(_) => false,
            StmtKind::Let { .. } => false,
            StmtKind::Block { body, .. } => {
                let mut does_return = false;
                for stmt in body {
                    if does_return {
                        return Err(TypeCheckerError::UnreachableCode { line: stmt.line });
                    }
                    does_return |= self.stmt_returns(stmt)?;
                }
                does_return
            }
            StmtKind::If {
                then_branch,
                else_branch,
                ..
            } => {
                if let Some(else_branch) = else_branch {
                    self.stmt_returns(else_branch)? && self.stmt_returns(then_branch)?
                } else {
                    false
                }
            }
            StmtKind::While { .. } => false,
            StmtKind::Function { .. } => false,
            StmtKind::Return(_) => true,
            StmtKind::Global { .. } => false,
            StmtKind::StructDecl { .. } => false,
            StmtKind::Impl { .. } => false,
        })
    }
}
