use crate::expression::Expression;
use crate::expression::Token;

pub trait Validation {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str>;
}

impl Validation for Expression {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str> {
        let mut current_input = input;
        for token in &self.tokens {
            match token.validate(current_input) {
                Some(new_input) => {
                    current_input = new_input;
                }
                None => return None,
            }
        }
        if self.end_anchor && !current_input.is_empty() {
            return None;
        }
        Some(current_input)
    }
}

impl Validation for Token {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str> {
        match self {
            Token::Tag(c) => input.strip_prefix(*c),
            Token::Digit => input.strip_prefix(|c: char| c.is_ascii_digit()),
            Token::AlphaNumeric => input.strip_prefix(|c: char| c.is_ascii_alphanumeric()),
            Token::PosCharGroup(group) => input.strip_prefix(|c: char| group.contains(&c)),
            Token::NegCharGroup(group) => input.strip_prefix(|c: char| !group.contains(&c)),
            Token::OneOrMore(c) => {
                let mut current_input = input.strip_prefix(*c)?;
                while let Some(new_input) = current_input.strip_prefix(*c) {
                    current_input = new_input;
                }
                Some(current_input)
            }
            Token::ZeroOrMore(c) => {
                let mut current_input = input;
                while let Some(new_input) = current_input.strip_prefix(*c) {
                    current_input = new_input;
                }
                Some(current_input)
            }
            Token::Wildcard => input.strip_prefix(|_| true),
            Token::Alternation((left, right)) => {
                if let Some(new_input) = left.validate(input) {
                    return Some(new_input);
                }
                right.validate(input)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_tag_validation() {
        let expression = Expression::from_str("abc").unwrap();
        assert!(expression.validate("abc").is_some());
        assert!(expression.validate("abcabc").is_some());
        assert!(expression.validate("aba").is_none());
        assert!(expression.validate("123").is_none());
        assert!(expression.validate("").is_none());
        assert!(expression.validate(" ").is_none());
        assert!(expression.validate("$!_").is_none());
    }

    #[test]
    fn test_digit_validation() {
        let expression = Expression::from_str("\\d").unwrap();
        assert!(expression.validate("1").is_some());
        assert!(expression.validate("123").is_some());
        assert!(expression.validate("ab1abc").is_none());
        assert!(expression.validate("").is_none());
        assert!(expression.validate(" ").is_none());
        assert!(expression.validate("$!_").is_none());
        assert!(expression.validate("a").is_none());
    }

    #[test]
    fn test_alpha_numeric_validation() {
        let expression = Expression::from_str("\\w").unwrap();
        assert!(expression.validate("1").is_some());
        assert!(expression.validate("a").is_some());
        assert!(expression.validate("").is_none());
        assert!(expression.validate("$!-").is_none());
    }

    #[test]
    fn test_positive_char_group() {
        let expression = Expression::from_str("[abc]").unwrap();
        assert!(expression.validate("a").is_some());
        assert!(expression.validate("b").is_some());
        assert!(expression.validate("c").is_some());
        assert!(expression.validate("ab").is_some());
        assert!(expression.validate("da").is_none());
        assert!(expression.validate("").is_none());
        assert!(expression.validate(" ").is_none());
        assert!(expression.validate("$![]").is_none());
        assert!(expression.validate("1").is_none());
    }

    #[test]
    fn test_negative_char_group() {
        let expression = Expression::from_str("[^abc]").unwrap();
        assert!(expression.validate("a").is_none());
        assert!(expression.validate("b").is_none());
        assert!(expression.validate("c").is_none());
        assert!(expression.validate("ab").is_none());
        assert!(expression.validate("da").is_some());
        assert!(expression.validate("def").is_some());
        assert!(expression.validate("").is_none());
        assert!(expression.validate(" ").is_some());
        assert!(expression.validate("$![]").is_some());
        assert!(expression.validate("1").is_some());
    }

    #[test]
    fn test_start_anchor() {
        let expression = Expression::from_str("^abc").unwrap();
        assert!(expression.validate("abc").is_some());
        assert!(expression.validate("abcc").is_some());
        assert!(expression.validate("aabc").is_none());
        assert!(expression.validate("^abc").is_none());
    }

    #[test]
    fn test_end_anchor() {
        let expression = Expression::from_str("abc$").unwrap();
        assert!(expression.validate("abc").is_some());
        assert!(expression.validate("aabc").is_none());
        assert!(expression.validate("abcc").is_none());
        assert!(expression.validate("abc$").is_none());
    }

    #[test]
    fn test_one_or_more() {
        let expression = Expression::from_str("ab+").unwrap();
        assert!(expression.validate("ab").is_some());
        assert!(expression.validate("abbbbb").is_some());
        assert!(expression.validate("cbb").is_none());
    }

    #[test]
    fn test_zero_or_more() {
        let expression = Expression::from_str("ab?").unwrap();
        assert!(expression.validate("a").is_some());
        assert!(expression.validate("abc").is_some());
        assert!(expression.validate("abbbbb").is_some());
    }

    #[test]
    fn test_wildcard() {
        let expression = Expression::from_str(".").unwrap();
        assert!(expression.validate("a").is_some());
        assert!(expression.validate("1").is_some());
        assert!(expression.validate(" ").is_some());
        assert!(expression.validate("$!").is_some());
        assert!(expression.validate("").is_none());
    }

    #[test]
    fn test_alternation() {
        let expression = Expression::from_str("(a|b)").unwrap();
        assert!(expression.validate("a").is_some());
        assert!(expression.validate("b").is_some());
        assert!(expression.validate("c").is_none());
    }
}
