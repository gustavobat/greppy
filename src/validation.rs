use crate::expression::Expression;
use crate::expression::Token;

pub trait Validation {
    fn feed_len(&self) -> usize;

    fn validate(&self, input: &str) -> bool;
}

impl Validation for Token {
    fn feed_len(&self) -> usize {
        match self {
            Token::Tag(s) => s.len(),
            Token::Digit => 1,
            Token::AlphaNumeric => 1,
        }
    }

    fn validate(&self, input: &str) -> bool {
        if input.len() < self.feed_len() {
            return false;
        }
        match self {
            Token::Tag(s) => input[..s.len()] == *s,
            Token::Digit => input.chars().next().unwrap().is_ascii_digit(),
            Token::AlphaNumeric => input.chars().next().unwrap().is_alphanumeric(),
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
        if input.len() < feed_len {
            return false;
        };
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
    fn test_tag_validation() {
        let expression = Expression::from_str("abc").unwrap();
        assert!(expression.validate("abc"));
        assert!(expression.validate("abcabc"));
        assert!(!expression.validate("aba"));
        assert!(!expression.validate("123"));
        assert!(!expression.validate(""));
        assert!(!expression.validate(" "));
        assert!(!expression.validate("$!_"));
    }

    #[test]
    fn test_digit_validation() {
        let expression = Expression::from_str("\\d").unwrap();
        assert!(expression.validate("1"));
        assert!(expression.validate("123"));
        assert!(expression.validate("ab1abc"));
        assert!(!expression.validate(""));
        assert!(!expression.validate(" "));
        assert!(!expression.validate("$!_"));
        assert!(!expression.validate("a"));
    }

    #[test]
    fn test_alpha_numeric_validation() {
        let expression = Expression::from_str("\\w").unwrap();
        assert!(expression.validate("1"));
        assert!(expression.validate("a"));
        assert!(!expression.validate(""));
        assert!(!expression.validate("$!"));
    }
}
