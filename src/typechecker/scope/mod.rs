pub mod guards;
pub mod manager;
pub mod types;
pub mod variables;

use crate::scanner::Span;
use crate::typechecker::core::types::Type;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum FunctionContext {
    None,
    Function(Type, Span),
}
