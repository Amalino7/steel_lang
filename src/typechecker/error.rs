use crate::typechecker::types::Type;

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
    CalleeIsNotCallable {
        found: Type,
        line: u32,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        line: u32,
        message: &'static str,
    },
    ComplexTypeMismatch {
        message: String,
        line: u32,
    },
    InvalidReturnOutsideFunction {
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
    TooManyArguments {
        callee: String,
        expected: usize,
        found: usize,
        line: u32,
    },
    DuplicateArgument {
        name: String,
        line: u32,
    },
    UndefinedParameter {
        param_name: String,
        callee: String,
        line: u32,
    },
    MissingArgument {
        param_name: String,
        callee: String,
        line: u32,
    },
    PositionalArgumentAfterNamed {
        callee: String,
        message: &'static str,
        line: u32,
    },
    InvalidTupleIndex {
        tuple_type: Type,
        index: String,
        line: u32,
    },
    UnreachablePattern {
        line: u32,
        message: String,
    },
    InvalidIsUsage {
        line: u32,
        message: &'static str,
    },
    MissingGeneric {
        ty_name: String,
        generic_name: String,
        line: u32,
    },
    CannotInferType {
        line: u32,
        uninferred_generics: Vec<String>,
    },
    InvalidGenericSpecification {
        line: u32,
        message: String,
    },
}

impl TypeCheckerError {
    pub fn get_line(&self) -> u32 {
        match self {
            TypeCheckerError::UndefinedType { line, .. } => *line,
            TypeCheckerError::UndefinedVariable { line, .. } => *line,
            TypeCheckerError::CalleeIsNotCallable { line, .. } => *line,
            TypeCheckerError::TypeMismatch { line, .. } => *line,
            TypeCheckerError::InvalidReturnOutsideFunction { line, .. } => *line,
            TypeCheckerError::UnreachableCode { line, .. } => *line,
            TypeCheckerError::MissingReturnStatement { line, .. } => *line,
            TypeCheckerError::AssignmentToCapturedVariable { line, .. } => *line,
            TypeCheckerError::TypeHasNoFields { line, .. } => *line,
            TypeCheckerError::UndefinedField { line, .. } => *line,
            TypeCheckerError::StructOutsideOfGlobalScope { line, .. } => *line,
            TypeCheckerError::UndefinedMethod { line, .. } => *line,
            TypeCheckerError::StaticMethodOnInstance { line, .. } => *line,
            TypeCheckerError::Redeclaration { line, .. } => *line,
            TypeCheckerError::DoesNotImplementInterface { line, .. } => *line,
            TypeCheckerError::UncoveredPattern { line, .. } => *line,
            TypeCheckerError::TooManyArguments { line, .. } => *line,
            TypeCheckerError::DuplicateArgument { line, .. } => *line,
            TypeCheckerError::UndefinedParameter { line, .. } => *line,
            TypeCheckerError::MissingArgument { line, .. } => *line,
            TypeCheckerError::PositionalArgumentAfterNamed { line, .. } => *line,
            TypeCheckerError::InvalidTupleIndex { line, .. } => *line,
            TypeCheckerError::UnreachablePattern { line, .. } => *line,
            TypeCheckerError::InvalidIsUsage { line, .. } => *line,
            TypeCheckerError::MissingGeneric { line, .. } => *line,
            TypeCheckerError::CannotInferType { line, .. } => *line,
            TypeCheckerError::InvalidGenericSpecification { line, .. } => *line,
            TypeCheckerError::ComplexTypeMismatch { line, .. } => *line,
        }
    }
}

impl std::fmt::Display for TypeCheckerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeCheckerError::UndefinedVariable { name, line } => {
                write!(f, "[line {}] Error: Undefined variable '{}'.", line, name)
            }
            TypeCheckerError::CalleeIsNotCallable { found, line } => {
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
            TypeCheckerError::InvalidReturnOutsideFunction { line } => {
                write!(
                    f,
                    "[line {}] Error: 'return' statement outside of a function body.",
                    line
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
            TypeCheckerError::DuplicateArgument { name, line } => {
                write!(
                    f,
                    "[line {}] Error: Duplicate argument name '{}'.",
                    line, name
                )
            }
            TypeCheckerError::UndefinedParameter {
                param_name,
                callee,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Undefined parameter '{}' in function '{}'.",
                    line, param_name, callee
                )
            }

            TypeCheckerError::TooManyArguments {
                callee,
                expected,
                found,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Too many arguments for function '{}'. Expected {} but found {}.",
                    line, callee, expected, found
                )
            }
            TypeCheckerError::MissingArgument {
                param_name,
                callee,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Missing argument '{}' in function '{}'.",
                    line, param_name, callee
                )
            }
            TypeCheckerError::PositionalArgumentAfterNamed {
                callee,
                message,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Positional argument after named in function '{}'. {}",
                    line, callee, message
                )
            }
            TypeCheckerError::InvalidTupleIndex {
                tuple_type,
                index,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Invalid tuple index '{}' of {}.",
                    line, index, tuple_type
                )
            }
            TypeCheckerError::UnreachablePattern { line, message } => {
                write!(
                    f,
                    "[line {}] Warning: Unreachable Pattern.\n {}",
                    line, message
                )
            }
            TypeCheckerError::InvalidIsUsage { line, message } => {
                write!(
                    f,
                    "[line {}] Error: Invalid usage of 'is' operator.\n {}",
                    line, message
                )
            }
            TypeCheckerError::MissingGeneric {
                ty_name,
                generic_name,
                line,
            } => {
                write!(
                    f,
                    "[line {}] Error: Missing generic on type '{}' name: {}",
                    line, ty_name, generic_name
                )
            }
            TypeCheckerError::CannotInferType {
                line,
                uninferred_generics,
            } => {
                write!(
                    f,
                    "[line {}] Error: Cannot infer generic type. {} Use explicit annotations.",
                    line,
                    uninferred_generics.join(", ")
                )
            }
            TypeCheckerError::InvalidGenericSpecification { line, message } => {
                write!(
                    f,
                    "[line {}] Error: Invalid generic specification.\n {}",
                    line, message
                )
            }
            TypeCheckerError::ComplexTypeMismatch { line, message } => {
                write!(f, "[line {}] Error: {}\n", line, message)
            }
        }
    }
}
