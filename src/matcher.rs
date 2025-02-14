use crate::regex::Regex;
use crate::size_hint::CalculateSizeHint;
use colored::Colorize;
use std::io::Write;
use validation::Validation;

mod error;
pub(crate) mod validation;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct MatchResults {
    pub input: String,
    pub matches: Vec<SourceSpan>,
}

impl MatchResults {
    pub fn print_result(&self, mut writer: impl Write) -> std::io::Result<()> {
        let mut cur_pos = 0;
        for m in &self.matches {
            let start = m.start;
            let end = m.end;
            let prefix = &self.input[cur_pos..start];
            let matched = &self.input[start..end];
            cur_pos = end;
            write!(writer, "{}{}", prefix, matched.red().bold())?;
        }
        writeln!(writer, "{}", &self.input[cur_pos..])?;
        Ok(())
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

    pub fn solve(&self) -> MatchResults {
        let search_space = self.gen_search_space();
        let mut matches: Vec<SourceSpan> = Vec::new();
        for (start, end) in search_space {
            let substring = &self.input[start..end];
            let res = &self.regex.expression.validate(substring);
            if res.is_some() {
                let new_match = SourceSpan::new(start, end);
                let last = matches.last_mut();
                if let Some(last) = last {
                    if let Some(merged) = last.merge(&new_match) {
                        *last = merged;
                    } else {
                        matches.push(new_match);
                    }
                } else {
                    matches.push(new_match);
                }
            }
        }
        MatchResults {
            input: self.input.clone(),
            matches,
        }
    }

    fn gen_search_space(&self) -> Vec<(usize, usize)> {
        if self.input.is_empty() {
            return vec![];
        }

        let starts = if self.regex.start_anchor {
            0..1
        } else {
            0..self.input.len()
        };

        let size_hint = self.regex.size_hint();

        if self.regex.end_anchor {
            return starts
                .filter(|start| {
                    let end = self.input.len();
                    let len = end - start;
                    size_hint.is_compatible(len)
                })
                .map(|start| (start, self.input.len()))
                .collect();
        }

        let mut search_space = Vec::new();
        for start in starts {
            for end in start..self.input.len() + 1 {
                let len = end - start;
                if size_hint.is_compatible(len) {
                    search_space.push((start, end));
                }
            }
        }
        search_space
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl SourceSpan {
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        SourceSpan { start, end }
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;
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

    #[test_case("a", "a", vec![SourceSpan::new(0, 1)]; "single char complete match")]
    #[test_case("a", "ab", vec![SourceSpan::new(0, 1)]; "single char partial match")]
    #[test_case("\\w", "a-c", vec![SourceSpan::new(0, 1), SourceSpan::new(2,3)]; "matched twice")]
    #[test_case("\\w", "alph4-num3ric", vec![SourceSpan::new(0, 5), SourceSpan::new(6, 13)]; "complex input")]
    fn test_solver(regex: &str, input: &str, expected_matches: Vec<SourceSpan>) {
        let regex = Regex::from_str(regex).unwrap();
        let solver = Matcher::new(regex, input.to_string());
        let result = solver.solve();
        assert_eq!(result.matches, expected_matches);
    }
}
