use crate::typechecker::type_ast::Type;

#[derive(Debug)]
pub enum TypeCheckerError {
    UndefinedType {
        name: String,
        line: u32,
    },
    UndefinedVariable {
        name: String,
        line: u32,
    },
    CalleeIsNotAFunction {
        found: Type,
        line: u32,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        line: u32,
        message: &'static str,
    },
    IncorrectArity {
        callee_name: String,
        expected: usize,
        found: usize,
        line: u32,
    },
    InvalidReturnOutsideFunction {
        line: u32,
    },
    FunctionParameterTypeMismatch {
        expected: Type,
        found: Type,
        param_index: usize,
        line: u32,
    },
    UnreachableCode {
        line: u32,
    },
    MissingReturnStatement {
        line: u32,
    },
    AssignmentToCapturedVariable {
        name: String,
        line: u32,
    },
}
impl std::fmt::Display for TypeCheckerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeCheckerError::UndefinedVariable { name, line } => {
                write!(f, "[line {}] Error: Undefined variable '{}'.", line, name)
            }
            TypeCheckerError::CalleeIsNotAFunction { found, line } => {
                write!(
                    f,
                    "[line {}] Error: Cannot call type different from function.\n Found type '{}' where a function was expected.",
                    line, found
                )
            }
            TypeCheckerError::TypeMismatch {
                expected,
                found,
                line,
                message,
            } => {
                write!(
                    f,
                    "[line {}] Error: {}.\n Note: Expected '{}' but found '{}'.",
                    line, message, expected, found
                )
            }
            TypeCheckerError::IncorrectArity {
                callee_name,
                expected,
                found,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Incorrect number of arguments for function '{}'. Expected {} but found {}.",
                    line, callee_name, expected, found
                )
            }
            TypeCheckerError::InvalidReturnOutsideFunction { line } => {
                write!(
                    f,
                    "[line {}] Error: 'return' statement outside of a function body.",
                    line
                )
            }
            TypeCheckerError::FunctionParameterTypeMismatch {
                expected,
                found,
                line,
                param_index,
            } => {
                write!(
                    f,
                    "[line {}] Error: Function parameter type mismatch for parameter in position '{}'. Expected '{}' but found '{}'.",
                    line, param_index, expected, found
                )
            }
            TypeCheckerError::UnreachableCode { line } => {
                write!(f, "[line {}] Error: Unreachable code detected.", line)
            }
            TypeCheckerError::MissingReturnStatement { line } => {
                write!(f, "[line {}] Error: Missing return statement.", line)
            }
            TypeCheckerError::AssignmentToCapturedVariable { name, line } => {
                write!(
                    f,
                    "[line {}] Error: Cannot assign to captured variable '{}'.",
                    line, name
                )
            }
            TypeCheckerError::UndefinedType { name, line } => {
                write!(f, "[line {}] Error: Undefined type '{}'.", line, name)
            }
        }
    }
}
