use crate::regex::lexer::TokenKind;
use thiserror::Error;

#[derive(Error, Clone, Debug, PartialEq, Eq)]
pub enum TokenError {
    #[error("Unsupported token: '{0}'")]
    Unsupported(String),
    #[error("Invalid position of anchor '{0}'")]
    InvalidAnchorPosition(char),
    #[error("Qualifier '{0}' must be preceded by another character")]
    QualifierNotPreceded(char),
    #[error("Unclosed bracket")]
    Unclosed,
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Expected {0:?} got {1}")]
    UnexpectedToken(TokenKind, String),
}

#[derive(Error, Clone, Debug, PartialEq, Eq)]
pub enum SyntaxError {
    #[error("Invalid token {0:?}")]
    InvalidToken(TokenError),
    #[error("Expected {0} got {1:?}")]
    Unexpected(String, TokenKind),
    #[error("Expected {0}")]
    Unexpected2(String),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Invalid atom start")]
    InvalidAtomStart,
}
