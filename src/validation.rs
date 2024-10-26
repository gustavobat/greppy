use crate::error::ValidationError;
use crate::expression::Expression;
use crate::expression::Token;
use crate::size_hint::SizeHintTrait;
use colored::Colorize;

type ValidationResult<'a> = Result<&'a str, ValidationError>;

pub trait Validation {
    fn validate<'a>(&self, input: &'a str) -> ValidationResult<'a>;
}

impl Validation for Expression {
    fn validate<'a>(&self, input: &'a str) -> ValidationResult<'a> {
        let search_space = gen_search_space(self, input);
        for (start, end) in search_space {
            let substring = &input[start..end];
            let res = validate_substring(substring, self);
            if res.is_ok() {
                print_result(input, start, end);
                return res;
            }
        }
        Err(ValidationError::InputMismatch)
    }
}

impl Validation for Token {
    fn validate<'a>(&self, input: &'a str) -> ValidationResult<'a> {
        match self {
            Token::Tag(c) => {
                if input.starts_with(*c) {
                    Ok(&input[1..])
                } else {
                    Err(ValidationError::InputMismatch)
                }
            }
            Token::Digit => {
                let Some(c) = input.chars().next() else {
                    return Err(ValidationError::InputMismatch);
                };
                if c.is_ascii_digit() {
                    Ok(&input[1..])
                } else {
                    Err(ValidationError::InputMismatch)
                }
            }
            Token::AlphaNumeric => {
                let Some(c) = input.chars().next() else {
                    return Err(ValidationError::InputMismatch);
                };
                if c.is_alphanumeric() {
                    Ok(&input[1..])
                } else {
                    Err(ValidationError::InputMismatch)
                }
            }
            Token::PosCharGroup(group) => {
                let Some(c) = input.chars().next() else {
                    return Err(ValidationError::InputMismatch);
                };
                if group.contains(&c) {
                    Ok(&input[1..])
                } else {
                    Err(ValidationError::InputMismatch)
                }
            }
            Token::NegCharGroup(group) => {
                let Some(c) = input.chars().next() else {
                    return Err(ValidationError::InputMismatch);
                };
                if !group.contains(&c) {
                    Ok(&input[1..])
                } else {
                    Err(ValidationError::InputMismatch)
                }
            }
            Token::OneOrMore(c) => {
                let mut current_input = input;
                if !current_input.starts_with(*c) {
                    return Err(ValidationError::InputMismatch);
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
            Token::Wildcard => {
                if input.is_empty() {
                    return Err(ValidationError::InputMismatch);
                }
                Ok(&input[1..])
            }
            Token::Alternation((left, right)) => {
                if let Ok(new_input) = validate_substring(input, &left.clone().into()) {
                    return Ok(new_input);
                }
                validate_substring(input, &right.clone().into())
            }
        }
    }
}

fn print_result(input: &str, start: usize, end: usize) {
    println!(
        "{}{}{}",
        &input[..start],
        &input[start..end].red().bold(),
        &input[end..]
    );
}

fn validate_substring<'a>(input: &'a str, expression: &Expression) -> ValidationResult<'a> {
    let mut current_input = input;
    for token in &expression.tokens {
        match token.validate(current_input) {
            Ok(new_input) => {
                current_input = new_input;
            }
            Err(_) => return Err(ValidationError::InputMismatch),
        }
    }
    if expression.end_anchor && !current_input.is_empty() {
        return Err(ValidationError::InputMismatch);
    }
    Ok(current_input)
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

    let size_hint = expression.size_hint();

    if expression.end_anchor {
        return starts
            .filter(|start| {
                let end = input.len();
                let len = end - start;
                size_hint.is_compatible(len)
            })
            .map(|start| (start, input.len()))
            .collect();
    }

    let mut search_space = Vec::new();
    for start in starts {
        for end in start..input.len() + 1 {
            let len = end - start;
            if size_hint.is_compatible(len) {
                search_space.push((start, end));
            }
        }
    }
    search_space
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_tag_validation() {
        let expression = Expression::from_str("abc").unwrap();
        assert!(expression.validate("abc").is_ok());
        assert!(expression.validate("abcabc").is_ok());
        assert!(expression.validate("aba").is_err());
        assert!(expression.validate("123").is_err());
        assert!(expression.validate("").is_err());
        assert!(expression.validate(" ").is_err());
        assert!(expression.validate("$!_").is_err());
    }

    #[test]
    fn test_digit_validation() {
        let expression = Expression::from_str("\\d").unwrap();
        assert!(expression.validate("1").is_ok());
        assert!(expression.validate("123").is_ok());
        assert!(expression.validate("ab1abc").is_ok());
        assert!(expression.validate("").is_err());
        assert!(expression.validate(" ").is_err());
        assert!(expression.validate("$!_").is_err());
        assert!(expression.validate("a").is_err());
    }

    #[test]
    fn test_alpha_numeric_validation() {
        let expression = Expression::from_str("\\w").unwrap();
        assert!(expression.validate("1").is_ok());
        assert!(expression.validate("a").is_ok());
        assert!(expression.validate("").is_err());
        assert!(expression.validate("$!").is_err());
    }

    #[test]
    fn test_positive_char_group() {
        let expression = Expression::from_str("[abc]").unwrap();
        assert!(expression.validate("a").is_ok());
        assert!(expression.validate("b").is_ok());
        assert!(expression.validate("c").is_ok());
        assert!(expression.validate("ab").is_ok());
        assert!(expression.validate("da").is_ok());
        assert!(expression.validate("").is_err());
        assert!(expression.validate(" ").is_err());
        assert!(expression.validate("$![]").is_err());
        assert!(expression.validate("1").is_err());
    }

    #[test]
    fn test_negative_char_group() {
        let expression = Expression::from_str("[^abc]").unwrap();
        assert!(expression.validate("a").is_err());
        assert!(expression.validate("b").is_err());
        assert!(expression.validate("c").is_err());
        assert!(expression.validate("ab").is_err());
        assert!(expression.validate("da").is_ok());
        assert!(expression.validate("def").is_ok());
        assert!(expression.validate("").is_err());
        assert!(expression.validate(" ").is_ok());
        assert!(expression.validate("$![]").is_ok());
        assert!(expression.validate("1").is_ok());
    }

    #[test]
    fn test_start_anchor() {
        let expression = Expression::from_str("^abc").unwrap();
        assert!(expression.validate("abc").is_ok());
        assert!(expression.validate("abcc").is_ok());
        assert!(expression.validate("aabc").is_err());
        assert!(expression.validate("^abc").is_err());
    }

    #[test]
    fn test_end_anchor() {
        let expression = Expression::from_str("abc$").unwrap();
        assert!(expression.validate("abc").is_ok());
        assert!(expression.validate("aabc").is_ok());
        assert!(expression.validate("abcc").is_err());
        assert!(expression.validate("abc$").is_err());
    }

    #[test]
    fn test_one_or_more() {
        let expression = Expression::from_str("ab+").unwrap();
        assert!(expression.validate("ab").is_ok());
        assert!(expression.validate("abbbbb").is_ok());
        assert!(expression.validate("cbb").is_err());
    }

    #[test]
    fn test_zero_or_more() {
        let expression = Expression::from_str("ab?").unwrap();
        assert!(expression.validate("a").is_ok());
        assert!(expression.validate("abc").is_ok());
        assert!(expression.validate("abbbbb").is_ok());
    }

    #[test]
    fn test_wildcard() {
        let expression = Expression::from_str(".").unwrap();
        assert!(expression.validate("a").is_ok());
        assert!(expression.validate("1").is_ok());
        assert!(expression.validate(" ").is_ok());
        assert!(expression.validate("$!").is_ok());
        assert!(expression.validate("").is_err());
    }

    #[test]
    fn test_alternation() {
        let expression = Expression::from_str("(a|b)").unwrap();
        assert!(expression.validate("a").is_ok());
        assert!(expression.validate("b").is_ok());
        assert!(expression.validate("c").is_err());
    }
}
