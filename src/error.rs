use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ExpressionError {
    #[error("Unsupported token: '{0}'")]
    Unsupported(String),
    #[error("The provided expression is empty")]
    EmptyExpression,
    #[error("Invalid position of anchor '{0}'")]
    InvalidAnchorPosition(char),
    #[error("Invalid unpreceded qualifier '{0}'")]
    UnprecededQualifier(char),
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ValidationError {
    #[error("The provided input is empty")]
    EmptyInput,
    #[error("The expression does not match the input")]
    InputMismatch,
}
