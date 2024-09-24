use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ExpressionError {
    #[error("Unsupported token: '{0}'")]
    Unsupported(String),
    #[error("The provided expression is empty")]
    EmptyExpression,
}
