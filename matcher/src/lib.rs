mod size_hint;
mod validation;

use crate::size_hint::CalculateSizeHint;
use crate::size_hint::SizeHintState;
use regex::Regex;
use std::collections::BTreeMap;
use std::collections::HashSet;
use validation::Validation;

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct RegexMatch<'s> {
    pub original: &'s str,
    pub span: SourceSpan,
    pub captured_groups: BTreeMap<usize, SourceSpan>,
}

impl<'s> RegexMatch<'s> {
    pub fn new(original: &'s str) -> Self {
        Self {
            original,
            span: Default::default(),
            captured_groups: Default::default(),
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

    pub fn view(&self) -> &str {
        &self.original[self.span.start_byte(self.original)..self.span.end_byte(self.original)]
    }

    pub fn offset(&mut self, original: &'s str, len: usize) {
        self.original = original;
        self.span = self.span.shift(len);
        for span in self.captured_groups.values_mut() {
            *span = span.shift(len);
        }
    }
}

impl<'s> From<&RegexMatch<'s>> for HashSet<RegexMatch<'s>> {
    fn from(value: &RegexMatch<'s>) -> Self {
        HashSet::from([value.clone()])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matcher<'e, 's> {
    regex: &'e Regex,
    input: &'s str,
}

impl<'e, 's> Matcher<'e, 's>
where
    'e: 's,
{
    pub fn new(regex: &'e Regex, input: &'e str) -> Self {
        Self { regex, input }
    }

    pub fn solve(self) -> Vec<RegexMatch<'s>> {
        let search_space = self.gen_search_space();
        let mut matches: Vec<_> = search_space
            .iter()
            .flat_map(|span| {
                let substr = span.substr(self.input);
                let initial_match = RegexMatch::new(substr);
                self.regex
                    .validate(&initial_match)
                    .into_iter()
                    .map(|mut relative_match| {
                        relative_match.offset(self.input, span.start);
                        relative_match
                    })
            })
            .collect();

        self.deduplicate_matches(&mut matches);
        matches
    }

    fn deduplicate_matches(&self, matches: &mut Vec<RegexMatch<'s>>) {
        matches.sort_by(|a, b| {
            a.span
                .start
                .cmp(&b.span.start)
                .then(b.span.end.cmp(&a.span.end))
        });
        matches.dedup_by(|a, b| a.span == b.span || b.span.contains(&a.span));
    }

    pub fn merge_matches(matches: &[RegexMatch<'s>]) -> Vec<SourceSpan> {
        if matches.is_empty() {
            return vec![];
        }

        let mut merged = Vec::new();
        let mut current_start = matches[0].span.start;
        let mut current_end = matches[0].span.end;

        for m in &matches[1..] {
            if m.span.start == current_end {
                current_end = current_end.max(m.span.end);
            } else if m.span.start > current_end {
                merged.push(SourceSpan::new(current_start, current_end));
                current_start = m.span.start;
                current_end = m.span.end;
            }
        }
        merged.push(SourceSpan::new(current_start, current_end));
        merged
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
        let size_hints = self.regex.size_hint(&mut state);

        let mut search_space = Vec::new();
        for size_hint in size_hints {
            if self.regex.end_anchor {
                search_space.extend(starts.clone().filter_map(|start| {
                    let end = char_count;
                    let len = end - start;
                    if size_hint.is_compatible(len) {
                        Some(SourceSpan { start, end })
                    } else {
                        None
                    }
                }));
                continue;
            }

            for start in starts.clone() {
                for end in start + 1..=char_count {
                    let len = end - start;
                    if size_hint.is_compatible(len) {
                        search_space.push(SourceSpan { start, end });
                    }
                }
            }
        }
        search_space
    }
}

/// A span in the source string, represented as a range of character indices (not byte indices).
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
        s.chars().take(self.start).map(|c| c.len_utf8()).sum()
    }

    pub fn end_byte(&self, s: &str) -> usize {
        s.chars().take(self.end).map(|c| c.len_utf8()).sum()
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

    pub fn contains(&self, other: &SourceSpan) -> bool {
        self.start <= other.start && other.end <= self.end
    }

    pub fn char_count(&self) -> usize {
        self.end - self.start
    }

    pub fn advance_end_by(&self, n: usize) -> Self {
        Self {
            start: self.start,
            end: self.end + n,
        }
    }

    pub fn shift(&self, offset: usize) -> Self {
        Self {
            start: self.start + offset,
            end: self.end + offset,
        }
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

    #[test_case((0, 5), (1, 4), true; "b contained in a")]
    #[test_case((0, 5), (6, 10), false; "disjoint distant")]
    #[test_case((0, 5), (0, 5), true; "a contained in a")]
    fn test_contains(a: (usize, usize), b: (usize, usize), expected: bool) {
        let a = SourceSpan::new(a.0, a.1);
        let b = SourceSpan::new(b.0, b.1);
        assert_eq!(a.contains(&b), expected);
    }
}
