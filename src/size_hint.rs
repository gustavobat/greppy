use std::cmp::Ordering;
use std::ops::Add;

use crate::regex::Atom;
use crate::regex::CharClass;
use crate::regex::CharRange;
use crate::regex::Expression;
use crate::regex::Factor;
use crate::regex::Regex;
use crate::regex::Term;

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

#[derive(Clone, Debug, Default)]
pub struct SizeHintState {
    pub(crate) captured_groups: Vec<SizeHint>,
}

pub trait CalculateSizeHint {
    fn size_hint(&self, state: &mut SizeHintState) -> SizeHint;
}

impl CalculateSizeHint for Regex {
    fn size_hint(&self, state: &mut SizeHintState) -> SizeHint {
        self.expression.size_hint(state)
    }
}

impl CalculateSizeHint for Expression {
    fn size_hint(&self, state: &mut SizeHintState) -> SizeHint {
        match self {
            Expression::Term(term) => term.size_hint(state),
            Expression::Alternation(left, right) => {
                left.size_hint(state).max(right.size_hint(state))
            }
        }
    }
}

impl CalculateSizeHint for Term {
    fn size_hint(&self, state: &mut SizeHintState) -> SizeHint {
        match self {
            Term::Factor(factor) => factor.size_hint(state),
            Term::Concatenation(left, right) => left.size_hint(state) + right.size_hint(state),
        }
    }
}

impl CalculateSizeHint for Factor {
    fn size_hint(&self, state: &mut SizeHintState) -> SizeHint {
        match self {
            Factor::Atom(atom) => atom.size_hint(state),
            Factor::ZeroOrOne(_) => SizeHint::AtLeast(0),
            Factor::ZeroOrMore(_) => SizeHint::AtLeast(0),
            Factor::OneOrMore(atom) => match atom.size_hint(state) {
                SizeHint::Exact(size) => SizeHint::AtLeast(size),
                SizeHint::AtLeast(size) => SizeHint::AtLeast(size),
            },
        }
    }
}

impl CalculateSizeHint for Atom {
    fn size_hint(&self, state: &mut SizeHintState) -> SizeHint {
        match self {
            Atom::Char(_) => SizeHint::Exact(1),
            Atom::AnyChar => SizeHint::Exact(1),
            Atom::NormalClass(_) => SizeHint::Exact(1),
            Atom::NegatedClass(_) => SizeHint::Exact(1),
            Atom::Parentheses(expr) => {
                let size_hint = expr.size_hint(state);
                state.captured_groups.push(size_hint);
                size_hint
            }
            Atom::BackReference(n) => state.captured_groups[*n - 1],
        }
    }
}

impl CalculateSizeHint for CharClass {
    fn size_hint(&self, _state: &mut SizeHintState) -> SizeHint {
        SizeHint::Exact(1)
    }
}

impl CalculateSizeHint for CharRange {
    fn size_hint(&self, _state: &mut SizeHintState) -> SizeHint {
        SizeHint::Exact(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regex::Regex;
    use std::str::FromStr;
    use test_case::test_case;

    #[test_case("a", SizeHint::Exact(1); "single char")]
    #[test_case("ab", SizeHint::Exact(2); "multiple chars")]
    #[test_case("12", SizeHint::Exact(2); "numeric chars")]
    #[test_case("\\d", SizeHint::Exact(1); "digit char class")]
    #[test_case("\\w", SizeHint::Exact(1); "alphanumeric char class")]
    #[test_case("[ab]", SizeHint::Exact(1); "positive char group")]
    #[test_case("[^ab]", SizeHint::Exact(1); "negative char group")]
    #[test_case("a+", SizeHint::AtLeast(1); "one or more")]
    #[test_case("a?", SizeHint::AtLeast(0); "zero or one")]
    #[test_case("a*", SizeHint::AtLeast(0); "zero or more")]
    #[test_case(".", SizeHint::Exact(1); "wildcard")]
    #[test_case("(a|abc)", SizeHint::Exact(3); "alternation")]
    fn test_size_hint(regex: &str, expected: SizeHint) {
        let result = Regex::from_str(regex).unwrap();
        let mut state = SizeHintState::default();
        assert_eq!(result.size_hint(&mut state), expected);
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
