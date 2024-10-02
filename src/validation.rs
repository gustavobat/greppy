use crate::expression::Expression;
use crate::expression::Token;

type ValidationResult<'a> = Result<&'a str, ()>;

fn validate_token<'a>(input: &'a str, token: &Token) -> ValidationResult<'a> {
    match token {
        Token::Tag(c) => {
            if input.starts_with(*c) {
                Ok(&input[1..])
            } else {
                Err(())
            }
        }
        Token::Digit => {
            let Some(c) = input.chars().next() else {
                return Err(());
            };
            if c.is_ascii_digit() {
                Ok(&input[1..])
            } else {
                Err(())
            }
        }
        Token::AlphaNumeric => {
            let Some(c) = input.chars().next() else {
                return Err(());
            };
            if c.is_alphanumeric() {
                Ok(&input[1..])
            } else {
                Err(())
            }
        }
        Token::PosCharGroup(group) => {
            let Some(c) = input.chars().next() else {
                return Err(());
            };
            if group.contains(&c) {
                Ok(&input[1..])
            } else {
                Err(())
            }
        }
        Token::NegCharGroup(group) => {
            let Some(c) = input.chars().next() else {
                return Err(());
            };
            if !group.contains(&c) {
                Ok(&input[1..])
            } else {
                Err(())
            }
        }
        Token::OneOrMore(c) => {
            let mut current_input = input;
            if !current_input.starts_with(*c) {
                return Err(());
            }
            current_input = &current_input[1..];

            while current_input.starts_with(*c) {
                current_input = &current_input[1..];
            }
            Ok(current_input)
        }
        Token::ZeroOrMore(c) => {
            let mut current_input = input;
            while current_input.starts_with(*c) {
                current_input = &current_input[1..];
            }
            Ok(current_input)
        }
    }
}

fn validate_substring(input: &str, expression: &Expression) -> bool {
    let mut current_input = input;
    for token in &expression.tokens {
        match validate_token(current_input, token) {
            Ok(new_input) => {
                current_input = new_input;
            }
            Err(_) => {
                return false;
            }
        }
    }
    if expression.end_anchor && !current_input.is_empty() {
        return false;
    }
    true
}

fn gen_search_space(expression: &Expression, input: &str) -> Vec<(usize, usize)> {
    if input.is_empty() {
        return vec![];
    }

    let starts = if expression.start_anchor {
        0..1
    } else {
        0..input.len()
    };

    if expression.end_anchor {
        return starts.map(move |i| (i, input.len())).collect();
    }

    let mut search_space = Vec::new();
    for start in starts {
        for end in start..input.len() + 1 {
            search_space.push((start, end));
        }
    }
    search_space
}

pub trait Validation {
    fn validate(&self, input: &str) -> bool;
}

impl Validation for Expression {
    fn validate(&self, input: &str) -> bool {
        let search_space = gen_search_space(self, input);
        for (start, end) in search_space {
            let substring = &input[start..end];
            if validate_substring(substring, self) {
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

    #[test]
    fn test_one_or_more() {
        let expression = Expression::from_str("ab+").unwrap();
        assert!(expression.validate("ab"));
        assert!(expression.validate("abbbbb"));
        assert!(!expression.validate("cbb"));
    }

    #[test]
    fn test_zero_or_more() {
        let expression = Expression::from_str("ab?").unwrap();
        assert!(expression.validate("a"));
        assert!(expression.validate("abc"));
        assert!(expression.validate("abbbbb"));
    }
}
