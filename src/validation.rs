use crate::error::ValidationError;
use crate::expression::Expression;
use crate::expression::Token;

pub(crate) type ValidationResult<'a> = Result<&'a str, ValidationError>;

pub trait Validation {
    fn validate<'a>(&self, input: &'a str) -> ValidationResult<'a>;
}

impl Validation for Expression {
    fn validate<'a>(&self, input: &'a str) -> ValidationResult<'a> {
        let mut current_input = input;
        for token in &self.tokens {
            match token.validate(current_input) {
                Ok(new_input) => {
                    current_input = new_input;
                }
                Err(_) => return Err(ValidationError::InputMismatch),
            }
        }
        if self.end_anchor && !current_input.is_empty() {
            return Err(ValidationError::InputMismatch);
        }
        Ok(current_input)
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
                if let Ok(new_input) = left.validate(input) {
                    return Ok(new_input);
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
        assert!(expression.validate("ab1abc").is_err());
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
        assert!(expression.validate("da").is_err());
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
        assert!(expression.validate("aabc").is_err());
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
