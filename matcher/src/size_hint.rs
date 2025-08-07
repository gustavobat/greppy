use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::ops::Add;

use regex::Atom;
use regex::CharClass;
use regex::CharRange;
use regex::Expression;
use regex::Factor;
use regex::Regex;
use regex::Term;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SizeHint {
    Exact(usize),
    AtLeast(usize),
}

impl SizeHint {
    pub(crate) fn is_compatible(&self, len: usize) -> bool {
        match self {
            SizeHint::Exact(size) => *size == len,
            SizeHint::AtLeast(size) => *size <= len,
        }
    }

    pub(crate) fn inner(self) -> usize {
        match self {
            SizeHint::Exact(size) => size,
            SizeHint::AtLeast(size) => size,
        }
    }
}

impl Add for SizeHint {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let inner_sum = self.inner() + rhs.inner();
        match (self, rhs) {
            (SizeHint::Exact(_), SizeHint::Exact(_)) => SizeHint::Exact(inner_sum),
            _ => SizeHint::AtLeast(inner_sum),
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
    pub(crate) captured_groups: BTreeMap<usize, HashSet<SizeHint>>,
}

pub trait CalculateSizeHint {
    fn size_hint(&self, state: &mut SizeHintState) -> HashSet<SizeHint>;
}

impl CalculateSizeHint for Regex {
    fn size_hint(&self, state: &mut SizeHintState) -> HashSet<SizeHint> {
        self.expression.size_hint(state)
    }
}

impl CalculateSizeHint for Expression {
    fn size_hint(&self, state: &mut SizeHintState) -> HashSet<SizeHint> {
        match self {
            Expression::Term(term) => term.size_hint(state),
            Expression::Alternation(left, right) => {
                let mut sh = left.size_hint(&mut state.clone());
                sh.extend(right.size_hint(state));
                sh
            }
        }
    }
}

impl CalculateSizeHint for Term {
    fn size_hint(&self, state: &mut SizeHintState) -> HashSet<SizeHint> {
        match self {
            Term::Factor(factor) => factor.size_hint(state),
            Term::Concatenation(left, right) => {
                let mut new = HashSet::new();
                for left in left.size_hint(state) {
                    for right in right.size_hint(state) {
                        new.insert(left + right);
                    }
                }
                new
            }
        }
    }
}

impl CalculateSizeHint for Factor {
    fn size_hint(&self, state: &mut SizeHintState) -> HashSet<SizeHint> {
        match self {
            Factor::Atom(atom) => atom.size_hint(state),
            Factor::ZeroOrOne(_) => HashSet::from([SizeHint::AtLeast(0)]),
            Factor::ZeroOrMore(_) => HashSet::from([SizeHint::AtLeast(0)]),
            Factor::OneOrMore(atom) => {
                let atom_results = atom.size_hint(state);
                let min_inner = atom_results
                    .iter()
                    .min()
                    .expect("Atom size hint should never be empty")
                    .inner();
                HashSet::from([SizeHint::AtLeast(min_inner)])
            }
        }
    }
}

impl CalculateSizeHint for Atom {
    fn size_hint(&self, state: &mut SizeHintState) -> HashSet<SizeHint> {
        match self {
            Atom::Parentheses { id, expr } => {
                let size_hint = expr.size_hint(state);
                state.captured_groups.insert(*id, size_hint.clone());
                size_hint
            }
            Atom::BackReference(n) => state
                .captured_groups
                .get(n)
                .expect("Backreferences must be resolved after their corresponding captured group")
                .clone(),
            _ => HashSet::from([SizeHint::Exact(1)]),
        }
    }
}

impl CalculateSizeHint for CharClass {
    fn size_hint(&self, _state: &mut SizeHintState) -> HashSet<SizeHint> {
        HashSet::from([SizeHint::Exact(1)])
    }
}

impl CalculateSizeHint for CharRange {
    fn size_hint(&self, _state: &mut SizeHintState) -> HashSet<SizeHint> {
        HashSet::from([SizeHint::Exact(1)])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;
    use std::str::FromStr;
    use test_case::test_case;

    #[test]
    fn is_compatible() {
        let exact = SizeHint::Exact(5);
        let at_least = SizeHint::AtLeast(3);

        assert!(exact.is_compatible(5));
        assert!(!exact.is_compatible(4));
        assert!(at_least.is_compatible(3));
        assert!(at_least.is_compatible(5));
        assert!(!at_least.is_compatible(2));
    }

    #[test_case(SizeHint::Exact(1), SizeHint::Exact(1), SizeHint::Exact(2); "exact add")]
    #[test_case(SizeHint::Exact(1), SizeHint::AtLeast(1), SizeHint::AtLeast(2); "exact & at least")]
    #[test_case(SizeHint::AtLeast(1), SizeHint::Exact(1), SizeHint::AtLeast(2); "at least & exact")]
    #[test_case(SizeHint::AtLeast(1), SizeHint::AtLeast(1), SizeHint::AtLeast(2); "at least add")]
    fn test_add_impl(a: SizeHint, b: SizeHint, expected: SizeHint) {
        assert_eq!(a + b, expected);
    }

    #[test_case(SizeHint::Exact(1), SizeHint::Exact(1), Ordering::Equal; "exact equal")]
    #[test_case(SizeHint::Exact(1), SizeHint::Exact(2), Ordering::Less; "exact less")]
    #[test_case(SizeHint::Exact(2), SizeHint::Exact(1), Ordering::Greater; "exact greater")]
    #[test_case(SizeHint::AtLeast(1), SizeHint::AtLeast(1), Ordering::Equal; "at least equal")]
    #[test_case(SizeHint::AtLeast(1), SizeHint::AtLeast(2), Ordering::Less; "at least less")]
    #[test_case(SizeHint::AtLeast(2), SizeHint::AtLeast(1), Ordering::Greater; "at least greater")]
    #[test_case(SizeHint::Exact(10), SizeHint::AtLeast(1), Ordering::Less; "exact less than at least"
    )]
    fn test_ord_impl(a: SizeHint, b: SizeHint, expected: Ordering) {
        assert_eq!(a.cmp(&b), expected);
    }

    #[test_case(SizeHint::Exact(1), SizeHint::Exact(1), Some(Ordering::Equal); "exact equal")]
    #[test_case(SizeHint::Exact(1), SizeHint::Exact(2), Some(Ordering::Less); "exact less")]
    #[test_case(SizeHint::Exact(2), SizeHint::Exact(1), Some(Ordering::Greater); "exact greater")]
    fn test_partial_ord_impl(a: SizeHint, b: SizeHint, expected: Option<Ordering>) {
        assert_eq!(a.partial_cmp(&b), expected);
    }

    #[test_case("a", &[SizeHint::Exact(1)]; "single char")]
    #[test_case("ab", &[SizeHint::Exact(2)]; "multiple chars")]
    #[test_case("12", &[SizeHint::Exact(2)]; "numeric chars")]
    #[test_case("\\d", &[SizeHint::Exact(1)]; "digit char class")]
    #[test_case("\\\\", &[SizeHint::Exact(1)]; "escaped backslash char")]
    #[test_case("\\w", &[SizeHint::Exact(1)]; "alphanumeric char class")]
    #[test_case("[ab]", &[SizeHint::Exact(1)]; "positive char group")]
    #[test_case("[^ab]", &[SizeHint::Exact(1)]; "negative char group")]
    #[test_case("a+", &[SizeHint::AtLeast(1)]; "one or more")]
    #[test_case("a?", &[SizeHint::AtLeast(0)]; "zero or one")]
    #[test_case("a*", &[SizeHint::AtLeast(0)]; "zero or more")]
    #[test_case(".", &[SizeHint::Exact(1)]; "wildcard")]
    #[test_case("(ab|(abc|abcd))", &[SizeHint::Exact(2), SizeHint::Exact(3), SizeHint::Exact(4)]; "alternation")]
    #[test_case("(abc)\\1", &[SizeHint::Exact(6)]; "backreference")]
    fn test_size_hint(regex: &str, expected: &[SizeHint]) {
        let result = Regex::from_str(regex).unwrap();
        let mut state = SizeHintState::default();
        assert_eq!(
            result.size_hint(&mut state),
            HashSet::from_iter(expected.iter().copied())
        );
    }
}
