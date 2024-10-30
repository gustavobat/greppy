use std::cmp::max;
use std::cmp::Ordering;
use std::ops::Add;

use crate::expression::Expression;
use crate::expression::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SizeHint {
    Exact(usize),
    AtLeast(usize),
}

impl SizeHint {
    pub fn is_compatible(&self, len: usize) -> bool {
        match self {
            SizeHint::Exact(size) => *size == len,
            SizeHint::AtLeast(size) => *size <= len,
        }
    }
}

impl Add for SizeHint {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (SizeHint::Exact(a), SizeHint::Exact(b)) => SizeHint::Exact(a + b),
            (SizeHint::Exact(a), SizeHint::AtLeast(b)) => SizeHint::AtLeast(a + b),
            (SizeHint::AtLeast(a), SizeHint::Exact(b)) => SizeHint::AtLeast(a + b),
            (SizeHint::AtLeast(a), SizeHint::AtLeast(b)) => SizeHint::AtLeast(a + b),
        }
    }
}

impl Ord for SizeHint {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (SizeHint::Exact(a), SizeHint::Exact(b)) => a.cmp(b),
            (SizeHint::Exact(_), SizeHint::AtLeast(_)) => Ordering::Less,
            (SizeHint::AtLeast(_), SizeHint::Exact(_)) => Ordering::Greater,
            (SizeHint::AtLeast(a), SizeHint::AtLeast(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for SizeHint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub trait CalculateSizeHint {
    fn size_hint(&self) -> SizeHint;
}

impl CalculateSizeHint for Token {
    fn size_hint(&self) -> SizeHint {
        match self {
            Token::Tag(_) => SizeHint::Exact(1),
            Token::Digit => SizeHint::Exact(1),
            Token::AlphaNumeric => SizeHint::Exact(1),
            Token::PosCharGroup(_) => SizeHint::Exact(1),
            Token::NegCharGroup(_) => SizeHint::Exact(1),
            Token::OneOrMore(_) => SizeHint::AtLeast(1),
            Token::ZeroOrMore(_) => SizeHint::AtLeast(0),
            Token::Wildcard => SizeHint::Exact(1),
            Token::Alternation((a, b)) => {
                let a_size = a.size_hint();
                let b_size = b.size_hint();
                max(a_size, b_size)
            }
        }
    }
}

impl CalculateSizeHint for Expression {
    fn size_hint(&self) -> SizeHint {
        self.tokens
            .iter()
            .fold(SizeHint::Exact(0), |acc, token| acc + token.size_hint())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;
    use test_case::test_case;

    #[test_case("a", SizeHint::Exact(1); "single char")]
    #[test_case("ab", SizeHint::Exact(2); "multiple chars")]
    #[test_case("12", SizeHint::Exact(2); "numeric chars")]
    #[test_case("\\d", SizeHint::Exact(1); "digit token")]
    #[test_case("\\w", SizeHint::Exact(1); "alphanumeric token")]
    #[test_case("[ab]", SizeHint::Exact(1); "positive char group")]
    #[test_case("[^ab]", SizeHint::Exact(1); "negative char group")]
    #[test_case("a+", SizeHint::AtLeast(1); "one or more")]
    #[test_case("a?", SizeHint::AtLeast(0); "zero or more")]
    #[test_case(".", SizeHint::Exact(1); "wildcard")]
    #[test_case("(a|abc)", SizeHint::Exact(3); "alternation")]
    fn test_size_hint(expression: &str, expected: SizeHint) {
        let result = Expression::from_str(expression).unwrap();
        assert_eq!(result.size_hint(), expected);
    }

    #[test_case(SizeHint::Exact(1), SizeHint::Exact(1), Ordering::Equal; "exact equal")]
    #[test_case(SizeHint::Exact(1), SizeHint::Exact(2), Ordering::Less; "exact less")]
    #[test_case(SizeHint::Exact(2), SizeHint::Exact(1), Ordering::Greater; "exact greater")]
    #[test_case(SizeHint::AtLeast(1), SizeHint::AtLeast(1), Ordering::Equal; "at least equal")]
    #[test_case(SizeHint::AtLeast(1), SizeHint::AtLeast(2), Ordering::Less; "at least less")]
    #[test_case(SizeHint::AtLeast(2), SizeHint::AtLeast(1), Ordering::Greater; "at least greater")]
    #[test_case(SizeHint::Exact(10), SizeHint::AtLeast(1), Ordering::Less; "exact less than at least")]
    fn test_ord_impl(a: SizeHint, b: SizeHint, expected: Ordering) {
        assert_eq!(a.cmp(&b), expected);
    }
}
