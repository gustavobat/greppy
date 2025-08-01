mod size_hint;
mod validation;

use crate::size_hint::CalculateSizeHint;
use crate::size_hint::SizeHintState;
use regex::Regex;
use std::collections::HashSet;
use validation::Validation;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct RegexMatch<'a> {
    pub original: &'a str,
    pub span: SourceSpan,
    pub captured_groups: Vec<Option<SourceSpan>>,
}

impl<'a> RegexMatch<'a> {
    pub fn new(
        original: &'a str,
        span: SourceSpan,
        captured_groups: Vec<Option<SourceSpan>>,
    ) -> Self {
        Self {
            original,
            span,
            captured_groups,
        }
    }

    pub fn advance_end_by(self, n: usize) -> Self {
        debug_assert!(self.span.end + n <= self.original.len());
        Self {
            original: self.original,
            span: SourceSpan {
                start: self.span.start,
                end: self.span.end + n,
            },
            captured_groups: self.captured_groups,
        }
    }
}

impl<'a> From<&RegexMatch<'a>> for HashSet<RegexMatch<'a>> {
    fn from(value: &RegexMatch<'a>) -> Self {
        HashSet::from([value.clone()])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matcher {
    regex: Regex,
    input: String,
}

impl Matcher {
    pub fn new(regex: Regex, input: String) -> Self {
        Self { regex, input }
    }

    pub fn solve(&self) -> HashSet<RegexMatch> {
        let search_space = self.gen_search_space();
        let mut matches = HashSet::new();
        for span in search_space {
            let input = span.substr(&self.input);
            let initial_match = RegexMatch::new(input, SourceSpan::default(), vec![]);
            let res = self.regex.validate(&initial_match);
            matches.extend(res.into_iter());
        }
        matches
    }

    fn gen_search_space(&self) -> Vec<SourceSpan> {
        if self.input.is_empty() {
            return vec![];
        }

        let char_count = self.input.chars().count();

        let starts = if self.regex.start_anchor {
            0..1
        } else {
            0..char_count
        };

        let mut state = SizeHintState::default();
        let size_hint = self.regex.size_hint(&mut state);

        if self.regex.end_anchor {
            return starts
                .filter_map(|start| {
                    let end = char_count;
                    let len = end - start;
                    if size_hint.is_compatible(len) {
                        Some(SourceSpan { start, end })
                    } else {
                        None
                    }
                })
                .collect();
        }

        let mut search_space = Vec::new();
        for start in starts {
            for end in start + 1..=char_count {
                let len = end - start;
                if size_hint.is_compatible(len) {
                    search_space.push(SourceSpan { start, end });
                }
            }
        }
        search_space
    }
}

#[derive(Clone, Debug, Default, Hash, Eq, PartialEq)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}

impl SourceSpan {
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        SourceSpan { start, end }
    }

    pub fn start_byte(&self, s: &str) -> usize {
        s.char_indices()
            .nth(self.start)
            .map(|(i, _)| i)
            .unwrap_or(s.len())
    }

    pub fn end_byte(&self, s: &str) -> usize {
        s.char_indices()
            .nth(self.end)
            .map(|(i, _)| i)
            .unwrap_or(s.len())
    }

    pub fn substr<'a>(&self, s: &'a str) -> &'a str {
        // This has to be done carefully to handle Unicode characters correctly.
        &s[self.start_byte(s)..self.end_byte(s)]
    }

    pub fn intersects(&self, other: &SourceSpan) -> bool {
        self.end > other.start && other.end > self.start
    }

    pub fn is_adjacent(&self, other: &SourceSpan) -> bool {
        self.end == other.start || other.end == self.start
    }

    pub fn merge(&self, other: &SourceSpan) -> Option<SourceSpan> {
        if self.intersects(other) || self.is_adjacent(other) {
            Some(SourceSpan {
                start: self.start.min(other.start),
                end: self.end.max(other.end),
            })
        } else {
            None
        }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_case::test_case;

    #[test_case((0, 5), (4, 10), true; "point intersection")]
    #[test_case((0, 5), (5, 10), false; "disjoint adjacent")]
    #[test_case((0, 5), (6, 10), false; "disjoint distant")]
    #[test_case((0, 5), (3, 6), true; "overlap")]
    #[test_case((0, 5), (2, 3), true; "b contained in a")]
    fn test_intersects(a: (usize, usize), b: (usize, usize), expected: bool) {
        let a = SourceSpan::new(a.0, a.1);
        let b = SourceSpan::new(b.0, b.1);
        assert_eq!(a.intersects(&b), expected);
        assert_eq!(b.intersects(&a), expected);
    }

    #[test_case((0, 5), (4, 10), false; "point intersection")]
    #[test_case((0, 5), (5, 10), true; "disjoint adjacent")]
    #[test_case((0, 5), (6, 10), false; "disjoint distant")]
    #[test_case((0, 5), (3, 6), false; "overlap")]
    #[test_case((0, 5), (2, 3), false; "b contained in a")]
    fn test_is_adjacent(a: (usize, usize), b: (usize, usize), expected: bool) {
        let a = SourceSpan::new(a.0, a.1);
        let b = SourceSpan::new(b.0, b.1);
        assert_eq!(a.is_adjacent(&b), expected);
        assert_eq!(b.is_adjacent(&a), expected);
    }

    #[test_case((0, 5), (4, 10), Some((0, 10)); "point intersection")]
    #[test_case((0, 5), (5, 10), Some((0, 10)); "disjoint adjacent")]
    #[test_case((0, 5), (6, 10), None; "disjoint distant")]
    #[test_case((0, 5), (3, 6), Some((0, 6)); "overlap")]
    #[test_case((0, 5), (2, 3), Some((0, 5)); "b contained in a")]
    fn test_merge(a: (usize, usize), b: (usize, usize), res: Option<(usize, usize)>) {
        let a = SourceSpan::new(a.0, a.1);
        let b = SourceSpan::new(b.0, b.1);
        let res = res.map(|(start, end)| SourceSpan::new(start, end));
        assert_eq!(a.merge(&b), res);
        assert_eq!(b.merge(&a), res);
    }
}
