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
            Token::PosCharGroup(_) => 1,
            Token::NegCharGroup(_) => 1,
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
            Token::PosCharGroup(group) => group.contains(&input.chars().next().unwrap()),
            Token::NegCharGroup(group) => !group.contains(&input.chars().next().unwrap()),
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

        if self.start_anchor {
            return validate_substring(&input[..feed_len], self);
        }

        let last_start = input.len() - feed_len;
        if self.end_anchor {
            return validate_substring(&input[last_start..], self);
        }

        for i in 0..=last_start {
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

    #[test]
    fn test_positive_char_group() {
        let expression = Expression::from_str("[abc]").unwrap();
        assert!(expression.validate("a"));
        assert!(expression.validate("b"));
        assert!(expression.validate("c"));
        assert!(expression.validate("ab"));
        assert!(expression.validate("da"));
        assert!(!expression.validate(""));
        assert!(!expression.validate(" "));
        assert!(!expression.validate("$![]"));
        assert!(!expression.validate("1"));
    }

    #[test]
    fn test_negative_char_group() {
        let expression = Expression::from_str("[^abc]").unwrap();
        assert!(!expression.validate("a"));
        assert!(!expression.validate("b"));
        assert!(!expression.validate("c"));
        assert!(!expression.validate("ab"));
        assert!(expression.validate("da"));
        assert!(expression.validate("def"));
        assert!(!expression.validate(""));
        assert!(expression.validate(" "));
        assert!(expression.validate("$![]"));
        assert!(expression.validate("1"));
    }

    #[test]
    fn test_start_anchor() {
        let expression = Expression::from_str("^abc").unwrap();
        assert!(expression.validate("abc"));
        assert!(expression.validate("abcc"));
        assert!(!expression.validate("aabc"));
        assert!(!expression.validate("^abc"));
    }

    #[test]
    fn test_end_anchor() {
        let expression = Expression::from_str("abc$").unwrap();
        assert!(expression.validate("abc"));
        assert!(expression.validate("aabc"));
        assert!(!expression.validate("abcc"));
        assert!(!expression.validate("abc$"));
    }
}
