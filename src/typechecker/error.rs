use crate::parser::ast::Type;

#[derive(Debug)]
pub enum TypeCheckerError {
    UndefinedVariable {
        name: String,
        line: usize,
    },
    CalleeIsNotAFunction {
        found: Type,
        line: usize,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        line: usize,
        message: &'static str,
    },
    IncorrectArity {
        callee_name: String,
        expected: usize,
        found: usize,
        line: usize,
    },
    InvalidReturnOutsideFunction {
        line: usize,
    },
    FunctionParameterTypeMismatch {
        expected: Type,
        found: Type,
        param_index: usize,
        line: usize,
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
        }
    }
}
