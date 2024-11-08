use crate::regex::Atom;
use crate::regex::CharClass;
use crate::regex::CharRange;
use crate::regex::Expression;
use crate::regex::Factor;
use crate::regex::Term;

pub trait Validation {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str>;
}

impl Validation for Expression {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str> {
        match self {
            Expression::Term(term) => term.validate(input),
            Expression::Alternation(term, expr) => {
                let left = term.validate(input);
                if let Some(new_input) = left {
                    return Some(new_input);
                }
                let right = expr.validate(input);
                if let Some(new_input) = right {
                    return Some(new_input);
                }
                None
            }
        }
    }
}

impl Validation for Term {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str> {
        match self {
            Term::Factor(factor) => factor.validate(input),
            Term::Concatenation(factor, term) => factor
                .validate(input)
                .and_then(|new_input| term.validate(new_input)),
        }
    }
}

impl Validation for Factor {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str> {
        match self {
            Factor::Atom(atom) => atom.validate(input),
            Factor::ZeroOrOne(atom) => atom.validate(input).or(Some(input)),
            Factor::ZeroOrMore(atom) => {
                let mut current_input = input;
                while let Some(new_input) = atom.validate(current_input) {
                    current_input = new_input;
                }
                Some(current_input)
            }
            Factor::OneOrMore(atom) => atom.validate(input).map(|mut current_input| {
                while let Some(new_input) = atom.validate(current_input) {
                    current_input = new_input;
                }
                current_input
            }),
        }
    }
}

impl Validation for Atom {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str> {
        match self {
            Atom::Char(c) => input.strip_prefix(*c),
            Atom::AnyChar => input.strip_prefix(|_| true),
            Atom::Parentheses(expr) => expr.validate(input),
            Atom::NormalClass(class) => class.validate(input),
            Atom::NegatedClass(class) => {
                let new_input = class.validate(input);
                if new_input.is_some() {
                    None
                } else {
                    Some(input)
                }
            }
        }
    }
}

impl Validation for CharClass {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str> {
        match self {
            CharClass::CharRange(range) => range.validate(input),
            CharClass::CharRangeClass(range, class) => {
                range.validate(input).or_else(|| class.validate(input))
            }
            CharClass::Alphanumeric => input.strip_prefix(|c: char| c.is_ascii_alphanumeric()),
            CharClass::Digit => input.strip_prefix(|c: char| c.is_ascii_digit()),
        }
    }
}

impl Validation for CharRange {
    fn validate<'a>(&self, input: &'a str) -> Option<&'a str> {
        match self {
            CharRange::Char(c) => input.strip_prefix(*c),
            CharRange::CharRange(start, end) => {
                input.strip_prefix(|c: char| *start <= c && c <= *end)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::matcher::Matcher;
    use crate::regex::Regex;
    use std::str::FromStr;

    fn is_match(regex: &Regex, input: &str) -> bool {
        let matcher = Matcher::new(regex.clone(), input.to_string());
        !matcher.solve().matches.is_empty()
    }

    #[test]
    fn test_char_class_validation() {
        let regex = Regex::from_str("abc").unwrap();
        assert!(is_match(&regex, "abc"));
        assert!(is_match(&regex, "abcabc"));
        assert!(!is_match(&regex, "aba"));
        assert!(!is_match(&regex, "123"));
        assert!(!is_match(&regex, ""));
        assert!(!is_match(&regex, " "));
        assert!(!is_match(&regex, "$!_"));
    }

    #[test]
    fn test_digit_validation() {
        let regex = Regex::from_str("\\d").unwrap();
        assert!(is_match(&regex, "1"));
        assert!(is_match(&regex, "123"));
        assert!(is_match(&regex, "ab1abc"));
        assert!(!is_match(&regex, ""));
        assert!(!is_match(&regex, " "));
        assert!(!is_match(&regex, "$!_"));
        assert!(!is_match(&regex, "a"));
    }

    #[test]
    fn test_alpha_numeric_validation() {
        let regex = Regex::from_str("\\w").unwrap();
        assert!(is_match(&regex, "1"));
        assert!(is_match(&regex, "a"));
        assert!(!is_match(&regex, ""));
        assert!(!is_match(&regex, "$!-"));
    }

    #[test]
    fn test_positive_char_group() {
        let regex = Regex::from_str("[abc]").unwrap();
        assert!(is_match(&regex, "a"));
        assert!(is_match(&regex, "b"));
        assert!(is_match(&regex, "c"));
        assert!(is_match(&regex, "ab"));
        assert!(is_match(&regex, "da"));
        assert!(!is_match(&regex, "d"));
        assert!(!is_match(&regex, ""));
        assert!(!is_match(&regex, " "));
        assert!(!is_match(&regex, "$![]"));
        assert!(!is_match(&regex, "1"));
    }

    #[test]
    fn test_negative_char_group() {
        let regex = Regex::from_str("[^abc]").unwrap();
        assert!(!is_match(&regex, "a"));
        assert!(!is_match(&regex, "b"));
        assert!(!is_match(&regex, "c"));
        assert!(!is_match(&regex, "ab"));
        assert!(is_match(&regex, "da"));
        assert!(is_match(&regex, "def"));
        assert!(!is_match(&regex, ""));
        assert!(is_match(&regex, " "));
        assert!(is_match(&regex, "$![]"));
        assert!(is_match(&regex, "1"));
    }

    #[test]
    fn test_start_anchor() {
        let regex = Regex::from_str("^abc").unwrap();
        assert!(is_match(&regex, "abc"));
        assert!(is_match(&regex, "abcc"));
        assert!(!is_match(&regex, "aabc"));
        assert!(!is_match(&regex, "^abc"));
    }

    #[test]
    fn test_end_anchor() {
        let regex = Regex::from_str("abc$").unwrap();
        assert!(is_match(&regex, "abc"));
        assert!(is_match(&regex, "aabc"));
        assert!(!is_match(&regex, "abcc"));
        assert!(!is_match(&regex, "abc$"));
    }

    #[test]
    fn test_one_or_more() {
        let regex = Regex::from_str("ab+").unwrap();
        assert!(is_match(&regex, "ab"));
        assert!(is_match(&regex, "abbbbb"));
        assert!(!is_match(&regex, "cbb"));
    }

    #[test]
    fn test_zero_or_more() {
        let regex = Regex::from_str("ab?").unwrap();
        assert!(is_match(&regex, "a"));
        assert!(is_match(&regex, "abc"));
        assert!(is_match(&regex, "abbbbb"));
    }

    #[test]
    fn test_wildcard() {
        let regex = Regex::from_str(".").unwrap();
        assert!(is_match(&regex, "a"));
        assert!(is_match(&regex, "1"));
        assert!(is_match(&regex, " "));
        assert!(is_match(&regex, "$!"));
        assert!(!is_match(&regex, ""));
    }

    #[test]
    fn test_alternation() {
        let regex = Regex::from_str("(a|b)").unwrap();
        assert!(is_match(&regex, "a"));
        assert!(is_match(&regex, "b"));
        assert!(!is_match(&regex, "c"));
    }
}
