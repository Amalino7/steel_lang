use crate::typechecker::type_ast::Type;

#[derive(Debug)]
pub enum TypeCheckerError {
    UndefinedType {
        name: String,
        line: u32,
        message: &'static str,
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
    TypeHasNoFields {
        found: Type,
        line: u32,
    },
    UndefinedField {
        struct_name: String,
        field_name: String,
        line: u32,
    },
    MissingField {
        struct_name: String,
        field_name: String,
        line: u32,
    },
    StructOutsideOfGlobalScope {
        name: String,
        line: u32,
    },
    UndefinedMethod {
        line: u32,
        found: Type,
        method_name: String,
    },
    StaticMethodOnInstance {
        method_name: String,
        line: u32,
    },
    Redeclaration {
        name: String,
        line: u32,
    },
    DoesNotImplementInterface {
        missing_methods: Vec<String>,
        interface: String,
        line: u32,
    },
    UncoveredPattern {
        variant: String,
        line: u32,
    },
}

impl TypeCheckerError {
    pub fn get_line(&self) -> u32 {
        match self {
            TypeCheckerError::UndefinedType { line, .. } => *line,
            TypeCheckerError::UndefinedVariable { line, .. } => *line,
            TypeCheckerError::CalleeIsNotAFunction { line, .. } => *line,
            TypeCheckerError::TypeMismatch { line, .. } => *line,
            TypeCheckerError::IncorrectArity { line, .. } => *line,
            TypeCheckerError::InvalidReturnOutsideFunction { line, .. } => *line,
            TypeCheckerError::FunctionParameterTypeMismatch { line, .. } => *line,
            TypeCheckerError::UnreachableCode { line, .. } => *line,
            TypeCheckerError::MissingReturnStatement { line, .. } => *line,
            TypeCheckerError::AssignmentToCapturedVariable { line, .. } => *line,
            TypeCheckerError::TypeHasNoFields { line, .. } => *line,
            TypeCheckerError::UndefinedField { line, .. } => *line,
            TypeCheckerError::MissingField { line, .. } => *line,
            TypeCheckerError::StructOutsideOfGlobalScope { line, .. } => *line,
            TypeCheckerError::UndefinedMethod { line, .. } => *line,
            TypeCheckerError::StaticMethodOnInstance { line, .. } => *line,
            TypeCheckerError::Redeclaration { line, .. } => *line,
            TypeCheckerError::DoesNotImplementInterface { line, .. } => *line,
            TypeCheckerError::UncoveredPattern { line, .. } => *line,
        }
    }
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
            TypeCheckerError::UndefinedType {
                name,
                line,
                message,
            } => {
                write!(
                    f,
                    "[line {}] Error: Undefined type '{}'.\n {}",
                    line, name, message
                )
            }
            TypeCheckerError::TypeHasNoFields { found, line } => {
                write!(f, "[line {}] Error: Type '{}' has no fields.", line, found)
            }
            TypeCheckerError::UndefinedField {
                struct_name,
                field_name,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Struct '{}' has no field '{}'.",
                    line, struct_name, field_name
                )
            }
            TypeCheckerError::MissingField {
                struct_name,
                field_name,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Struct '{}' is missing field '{}' from its initializer.",
                    line, struct_name, field_name
                )
            }
            TypeCheckerError::StructOutsideOfGlobalScope { name, line } => {
                write!(
                    f,
                    "[line {}] Error: Struct '{}' is outside of global scope.",
                    line, name
                )
            }
            TypeCheckerError::UndefinedMethod {
                line,
                found,
                method_name,
            } => {
                write!(
                    f,
                    "[line {}] Error: Undefined method '{}' on type {}.",
                    line, method_name, found
                )
            }
            TypeCheckerError::StaticMethodOnInstance { method_name, line } => {
                write!(
                    f,
                    "[line {}] Error: Cannot call static method '{}' on instance.",
                    line, method_name
                )
            }
            TypeCheckerError::Redeclaration { name, line } => {
                write!(
                    f,
                    "[line {}] Error: Redeclaration of type or variable '{}'.",
                    line, name
                )
            }
            TypeCheckerError::DoesNotImplementInterface {
                missing_methods,
                interface,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Type does not implement interface '{}'. Missing methods or mismatched types: {}.",
                    line,
                    interface,
                    missing_methods.join(", ")
                )
            }
            TypeCheckerError::UncoveredPattern { variant, line } => {
                write!(
                    f,
                    "[line {}] Error: Uncovered pattern matching variant '{}'.",
                    line, variant
                )
            }
        }
    }
}
