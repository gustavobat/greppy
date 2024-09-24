use std::str::FromStr;

use crate::error::ExpressionError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Char(char),
}

impl Token {
    pub fn feed_len(&self) -> usize {
        match self {
            Token::Char(_) => 1,
        }
    }

    pub fn validate(&self, input: &str) -> bool {
        if input.len() < self.feed_len() {
            return false;
        }
        match self {
            Token::Char(expected) => input.chars().next().unwrap() == *expected,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Expression {
    pub tokens: Vec<Token>,
}

impl Expression {
    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }

    pub fn feed_len(&self) -> usize {
        self.tokens.iter().map(Token::feed_len).sum()
    }
}

impl FromStr for Expression {
    type Err = ExpressionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Err(ExpressionError::EmptyExpression);
        }
        let mut expression = Expression::default();
        for c in s.chars() {
            if c.is_alphabetic() {
                expression.push(Token::Char(c));
            } else {
                return Err(ExpressionError::Unsupported(c.to_string()));
            }
        }
        Ok(expression)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use test_case::test_case;

    #[test_case("a", vec![Token::Char('a')]; "single char")]
    #[test_case("ab", vec![Token::Char('a'), Token::Char('b')]; "multiple chars")]
    fn test_expression(input: &str, expected_tokens: Vec<Token>) {
        let result = Expression::from_str(input).unwrap();
        assert_eq!(result.tokens, expected_tokens);
    }

    #[test]
    fn test_empty_expression() {
        let result = Expression::from_str("").unwrap_err();
        assert_eq!(result, ExpressionError::EmptyExpression);
    }

    #[test]
    fn test_unsupported_token() {
        let result = Expression::from_str("a1").unwrap_err();
        assert_eq!(result, ExpressionError::Unsupported("1".to_string()));
    }
}
