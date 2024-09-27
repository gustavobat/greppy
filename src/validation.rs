use crate::expression::Expression;

pub fn validate_input(input: &str, expression: &Expression) -> bool {
    let feed_len = expression.feed_len();
    for i in 0..input.len() - feed_len + 1 {
        if validate_substring(&input[i..i + feed_len], expression) {
            return true;
        }
    }
    false
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_validate_input() {
        let expression = Expression::from_str("abc").unwrap();
        assert!(validate_input("abc", &expression));
        assert!(validate_input("abcabc", &expression));
        assert!(validate_input("abca", &expression));
        assert!(!validate_input("aba", &expression));
    }
}
