use codegen::CodegenError;
use cranelift::{
    module::ModuleError,
    prelude::{isa::LookupError, settings::SetError, *},
};
use thiserror::Error;

/// Represents an error that may be thrown by any method in the compiler
#[derive(Error, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    IsaLookup(#[from] LookupError),

    #[error(transparent)]
    Module(#[from] ModuleError),

    #[error(transparent)]
    Codegen(#[from] CodegenError),

    #[error(transparent)]
    Set(#[from] SetError),

    #[error("Variable {0} does not exist")]
    Missing(String),

    #[error("Expression {0} should be a function, but is not")]
    NotAFunction(String),

    #[error("{0}")]
    InvalidType(String),

    #[error("Trying to mutate `{0}`, but it is immutable")]
    MutationError(String),
}
