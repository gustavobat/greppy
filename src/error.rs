use thiserror::Error;

#[derive(Error, Debug, PartialEq, Eq)]
pub enum ExpressionError {
    #[error("Unsupported token: '{0}'")]
    Unsupported(String),
    #[error("The provided expression is empty")]
    EmptyExpression,
    #[error("Invalid position of anchor '{0}'")]
    InvalidAnchorPosition(char),
    #[error("Qualifier '{0}' must be preceded by another character")]
    QualifierNotPreceded(char),
}
