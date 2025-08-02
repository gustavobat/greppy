use crate::RegexMatch;
use crate::SourceSpan;
use regex::Atom;
use regex::CharClass;
use regex::CharRange;
use regex::Expression;
use regex::Factor;
use regex::Regex;
use regex::Term;
use std::collections::HashSet;

pub trait Validation {
    fn validate<'a>(&self, old: &RegexMatch<'a>) -> HashSet<RegexMatch<'a>>;

    fn validate_all<'a>(&self, matches: HashSet<RegexMatch<'a>>) -> HashSet<RegexMatch<'a>> {
        matches
            .iter()
            .flat_map(|old_match| self.validate(old_match))
            .collect()
    }
}

impl Validation for Regex {
    fn validate<'a>(&self, initial: &RegexMatch<'a>) -> HashSet<RegexMatch<'a>> {
        let mut res = self.expression.validate(initial);
        if self.start_anchor {
            res.retain(|m| m.span.start == 0);
        }
        if self.end_anchor {
            res.retain(|m| m.span.end == initial.original.len());
        }
        res
    }
}

impl Validation for Expression {
    fn validate<'a>(&self, old: &RegexMatch<'a>) -> HashSet<RegexMatch<'a>> {
        match self {
            Expression::Term(term) => term.validate(old),
            Expression::Alternation(term, expr) => {
                let mut new = term.validate(old);
                new.extend(expr.validate(old));
                new
            }
        }
    }
}

impl Validation for Term {
    fn validate<'a>(&self, old: &RegexMatch<'a>) -> HashSet<RegexMatch<'a>> {
        match self {
            Term::Factor(factor) => factor.validate(old),
            Term::Concatenation(factor, term) => term.validate_all(factor.validate(old)),
        }
    }
}

impl Validation for Factor {
    fn validate<'a>(&self, old: &RegexMatch<'a>) -> HashSet<RegexMatch<'a>> {
        match self {
            Factor::Atom(atom) => atom.validate(old),
            Factor::ZeroOrOne(atom) => {
                let mut new = atom.validate(old);
                new.insert(old.clone());
                new
            }
            Factor::ZeroOrMore(atom) => {
                let mut new = HashSet::new();
                let mut current_input = HashSet::from([old.clone()]);
                while !current_input.is_empty() {
                    current_input = atom.validate_all(current_input);
                    new.extend(current_input.clone());
                }
                new.insert(old.clone());
                new
            }
            Factor::OneOrMore(atom) => {
                let mut new = HashSet::new();
                let mut current_input = HashSet::from([old.clone()]);
                while !current_input.is_empty() {
                    current_input = atom.validate_all(current_input);
                    new.extend(current_input.clone());
                }
                new
            }
        }
    }
}

impl Validation for Atom {
    fn validate<'a>(&self, old: &RegexMatch<'a>) -> HashSet<RegexMatch<'a>> {
        let mut new = HashSet::new();
        let Some(next_char) = old.original.chars().nth(old.span.end) else {
            return new;
        };
        match self {
            Atom::Char(c) => {
                if next_char == *c {
                    new.insert(old.clone().advance_end_by(1));
                }
            }
            Atom::Parentheses { id, expr } => {
                for mut new_match in expr.validate(&old.clone()).into_iter() {
                    let new_start = old.span.end;
                    let new_end = new_match.span.end;
                    new_match
                        .captured_groups
                        .insert(*id, SourceSpan::new(new_start, new_end));
                    new.insert(new_match);
                }
            }
            Atom::NormalClass(class) => {
                new.extend(class.validate(old));
            }
            Atom::NegatedClass(class) => {
                if class.validate(old).is_empty() {
                    new.insert(old.clone().advance_end_by(1));
                }
            }
            Atom::BackReference(n) => {
                let group = old.captured_groups.get(n).expect(
                    "Backreferences must be resolved after their corresponding captured group",
                );
                let captured = group.substr(old.original);
                let end_byte = old.span.end_byte(old.original);
                let new_end_byte = old
                    .original
                    .char_indices()
                    .nth(old.span.end + group.len())
                    .map(|(i, _)| i)
                    .unwrap_or(old.original.len());
                let rest = &old.original[end_byte..new_end_byte];
                if rest == captured {
                    new.insert(old.clone().advance_end_by(captured.len()));
                }
            }
        }
        new
    }
}

impl Validation for CharClass {
    fn validate<'a>(&self, old: &RegexMatch<'a>) -> HashSet<RegexMatch<'a>> {
        let mut new = HashSet::new();
        let Some(next_char) = old.original.chars().nth(old.span.end) else {
            return new;
        };
        match self {
            CharClass::CharRange(range) => new.extend(range.validate(old)),
            CharClass::CharRangeClass(range, class) => {
                new.extend(range.validate(old));
                new.extend(class.validate(old));
            }
            CharClass::WordChar => {
                if next_char.is_ascii_alphanumeric() || next_char == '_' {
                    new.insert(old.clone().advance_end_by(1));
                }
            }
            CharClass::Digit => {
                if next_char.is_ascii_digit() {
                    new.insert(old.clone().advance_end_by(1));
                }
            }
            CharClass::AnyChar => {
                new.insert(old.clone().advance_end_by(1));
            }
        }
        new
    }
}

impl Validation for CharRange {
    fn validate<'a>(&self, old: &RegexMatch<'a>) -> HashSet<RegexMatch<'a>> {
        let mut new = HashSet::new();
        let Some(next_char) = old.original.chars().nth(old.span.end) else {
            return new;
        };
        match self {
            CharRange::Char(c) => {
                if next_char == *c {
                    new.insert(old.clone().advance_end_by(1));
                }
            }
            CharRange::CharRange(start, end) => {
                if next_char >= *start && next_char <= *end {
                    new.insert(old.clone().advance_end_by(1));
                }
            }
        }
        new
    }
}

#[cfg(test)]
mod tests {
    use crate::Matcher;
    use regex::Regex;
    use std::str::FromStr;

    fn is_match(regex: &Regex, input: &str) -> bool {
        let matcher = Matcher::new(regex.clone(), input.to_string());
        !matcher.solve().is_empty()
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

        let regex = Regex::from_str("[^x]+").unwrap();
        assert!(is_match(&regex, "a"));
        assert!(is_match(&regex, "ab"));
        assert!(!is_match(&regex, "x"));
    }

    #[test]
    fn test_zero_or_one() {
        let regex = Regex::from_str("ab?").unwrap();
        assert!(is_match(&regex, "ac"));
        assert!(is_match(&regex, "ab"));
    }

    #[test]
    fn test_zero_or_more() {
        let regex = Regex::from_str("ab*").unwrap();
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

    #[test]
    fn test_back_reference() {
        let regex = Regex::from_str("(a)\\1").unwrap();
        assert!(is_match(&regex, "aa"));
        assert!(!is_match(&regex, "ab"));

        let regex = Regex::from_str("(\\w)\\1").unwrap();
        assert!(is_match(&regex, "aa"));
        assert!(is_match(&regex, "bb"));
        assert!(!is_match(&regex, "ab"));

        let regex = Regex::from_str("a(\\w\\w)\\1").unwrap();
        assert!(is_match(&regex, "aabab"));
        assert!(is_match(&regex, "ababa"));

        let regex = Regex::from_str("^I see (\\d (cat|dog|cow)(, | and )?)+$").unwrap();
        assert!(!is_match(&regex, "I see 1 cat, 2 dogs and 3 cows"));
        let regex = Regex::from_str(r"\d\\d\\dx").unwrap();
        assert!(!is_match(&regex, "12x"));

        let regex = Regex::from_str("^(cat) n \\1$").unwrap();
        assert!(is_match(&regex, "cat n cat"));
    }

    #[test]
    fn test_utf8() {
        let regex = Regex::from_str("🦀").unwrap();
        assert!(is_match(&regex, "🦀"));
        assert!(!is_match(&regex, "🦁"));
    }

    #[test]
    fn nested_back_ref() {
        let regex = Regex::from_str("((c)\\2)\\1").unwrap();
        assert!(is_match(&regex, "cccc"));
    }
}
