use crate::typechecker::core::error::{
    BindingError, CallError, CallParamError, CallParamKind, DuplicateDefinition, GenericError,
    TypeCheckerError,
};

impl TypeCheckerError {
    pub(crate) fn code(&self) -> &'static str {
        match self {
            TypeCheckerError::TypeMismatch { .. } => "E001",
            TypeCheckerError::OperatorConstraint { .. } => "E023",
            TypeCheckerError::UndefinedVariable { .. } => "E002",
            TypeCheckerError::UndefinedType { .. } => "E003",
            TypeCheckerError::UndefinedField { .. } => "E004",
            TypeCheckerError::UndefinedMethod(_) => "E005",
            TypeCheckerError::MissingReturnStatement { .. } => "E006",
            TypeCheckerError::InvalidReturnOutsideFunction { .. } => "E007",
            TypeCheckerError::CalleeIsNotCallable { .. } => "E008",
            TypeCheckerError::TypeHasNoFields { .. } => "E014",
            TypeCheckerError::SelfOutsideOfImpl { .. } => "E015",
            TypeCheckerError::NonGlobalDeclaration { .. } => "E017",
            TypeCheckerError::StaticMethodOnInstance { .. } => "E018",
            TypeCheckerError::MissingInterfaceMethods { .. } => "E020",
            TypeCheckerError::UncoveredPattern { .. } => "E021",
            TypeCheckerError::InvalidTupleIndex { .. } => "E022",
            TypeCheckerError::InvalidIsUsage { .. } => "E024",
            TypeCheckerError::InterfaceMethodTypeMismatch { .. } => "E028",
            TypeCheckerError::InvalidOperandTypes { .. } => "E030",
            TypeCheckerError::PrimitiveTypeShadowing { .. } => "E035",
            TypeCheckerError::Duplicate(d) => d.code(),
            TypeCheckerError::CallParam(c) => c.code(),
            TypeCheckerError::Call(c) => c.code(),
            TypeCheckerError::Generic(g) => g.code(),
            TypeCheckerError::Binding(b) => b.code(),
        }
    }
}

impl BindingError {
    fn code(&self) -> &'static str {
        match self {
            BindingError::Redeclaration { .. } => "E019",
            BindingError::Immutable { .. } => "E029",
            BindingError::Captured { .. } => "E016",
        }
    }
}

impl DuplicateDefinition {
    fn code(&self) -> &'static str {
        self.kind.code()
    }
}

impl CallParamError {
    fn code(&self) -> &'static str {
        match self.kind {
            CallParamKind::Missing => "E010",
            CallParamKind::Undefined => "E012",
        }
    }
}

impl CallError {
    fn code(&self) -> &'static str {
        match self {
            CallError::TooMany { .. } => "E009",
            CallError::DuplicateArgument { .. } => "E011",
            CallError::PositionalAfterNamed { .. } => "E013",
        }
    }
}

impl GenericError {
    fn code(&self) -> &'static str {
        match self {
            GenericError::CannotInfer { .. } => "E025",
            GenericError::CountMismatch { .. } => "E027",
            GenericError::InvalidSpecification { .. } => "E026",
        }
    }
}
