use crate::expression::{Expression, Token};

pub trait Validation {
    fn feed_len(&self) -> usize;

    fn validate(&self, input: &str) -> bool;
}

impl Validation for Token {
    fn feed_len(&self) -> usize {
        match self {
            Token::Char(_) => 1,
        }
    }

    fn validate(&self, input: &str) -> bool {
        if input.len() < self.feed_len() {
            return false;
        }
        match self {
            Token::Char(expected) => input.chars().next().unwrap() == *expected,
        }
    }
}

fn validate_substring(input: &str, expression: &Expression) -> bool {
    let mut current_input = input;
    for token in &expression.tokens {
        if !token.validate(current_input) {
            return false;
        }
        current_input = &current_input[token.feed_len()..];
    }
    true
}

impl Validation for Expression {
    fn feed_len(&self) -> usize {
        self.tokens.iter().map(Token::feed_len).sum()
    }

    fn validate(&self, input: &str) -> bool {
        let feed_len = self.feed_len();
        for i in 0..input.len() - feed_len + 1 {
            if validate_substring(&input[i..i + feed_len], self) {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_validate_input() {
        let expression = Expression::from_str("abc").unwrap();
        assert!(expression.validate("abc"));
        assert!(expression.validate("abcabc"));
        assert!(expression.validate("abca"));
        assert!(!expression.validate("aba"));
    }
}
