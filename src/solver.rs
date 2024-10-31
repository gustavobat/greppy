use crate::expression::Expression;
use crate::size_hint::CalculateSizeHint;
use crate::validation::Validation;
use colored::Colorize;

pub struct Solver {
    expression: Expression,
    input: String,
    matches: Vec<Match>,
}

impl Solver {
    pub fn new(expression: Expression, input: String) -> Self {
        Self {
            expression,
            input,
            matches: Vec::new(),
        }
    }

    pub fn solve(&mut self) -> bool {
        let search_space = self.gen_search_space();
        for (start, end) in search_space {
            let substring = &self.input[start..end];
            let res = &self.expression.validate(substring);
            if res.is_some() {
                let new_match = Match::new(start, end);
                let last = self.matches.last_mut();
                if let Some(last) = last {
                    if let Some(merged) = last.merge(&new_match) {
                        *last = merged;
                    } else {
                        self.matches.push(new_match);
                    }
                } else {
                    self.matches.push(new_match);
                }
            }
        }
        !self.matches.is_empty()
    }

    fn gen_search_space(&self) -> Vec<(usize, usize)> {
        if self.input.is_empty() {
            return vec![];
        }

        let starts = if self.expression.start_anchor {
            0..1
        } else {
            0..self.input.len()
        };

        let size_hint = self.expression.size_hint();

        if self.expression.end_anchor {
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

    pub fn print_result(&self) {
        let mut cur_pos = 0;
        for m in &self.matches {
            let start = m.start;
            let end = m.end;
            let prefix = &self.input[cur_pos..start];
            let matched = &self.input[start..end];
            cur_pos = end;
            print!("{}{}", prefix, matched.red().bold());
        }
        println!("{}", &self.input[cur_pos..]);
    }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct Match {
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl Match {
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        Match { start, end }
    }

    pub fn intersects(&self, other: &Match) -> bool {
        self.start <= other.end && self.end >= other.start
    }

    pub fn is_adjacent(&self, other: &Match) -> bool {
        self.start == other.end + 1 || other.start == self.end + 1
    }

    pub fn merge(&self, other: &Match) -> Option<Match> {
        if self.intersects(other) || self.is_adjacent(other) {
            Some(Match {
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
    use test_case::test_case;

    #[test_case((0, 5), (5, 10), true; "point intersection")]
    #[test_case((0, 5), (6, 10), false; "disjoint adjacent")]
    #[test_case((0, 5), (7, 10), false; "disjoint distant")]
    #[test_case((0, 5), (4, 6), true; "overlap")]
    #[test_case((0, 5), (2, 3), true; "b contained in a")]
    fn test_intersects(a: (usize, usize), b: (usize, usize), expected: bool) {
        let a = Match::new(a.0, a.1);
        let b = Match::new(b.0, b.1);
        assert_eq!(a.intersects(&b), expected);
        assert_eq!(b.intersects(&a), expected);
    }

    #[test_case((0, 5), (5, 10), false; "point intersection")]
    #[test_case((0, 5), (6, 10), true; "disjoint adjacent")]
    #[test_case((0, 5), (7, 10), false; "disjoint distant")]
    #[test_case((0, 5), (4, 6), false; "overlap")]
    #[test_case((0, 5), (2, 3), false; "b contained in a")]
    fn test_is_adjacent(a: (usize, usize), b: (usize, usize), expected: bool) {
        let a = Match::new(a.0, a.1);
        let b = Match::new(b.0, b.1);
        assert_eq!(a.is_adjacent(&b), expected);
        assert_eq!(b.is_adjacent(&a), expected);
    }

    #[test_case((0, 5), (5, 10), Some((0, 10)); "point intersection")]
    #[test_case((0, 5), (6, 10), Some((0, 10)); "disjoint adjacent")]
    #[test_case((0, 5), (7, 10), None; "disjoint distant")]
    #[test_case((0, 5), (4, 6), Some((0, 6)); "overlap")]
    #[test_case((0, 5), (2, 3), Some((0, 5)); "b contained in a")]
    fn test_merge(a: (usize, usize), b: (usize, usize), res: Option<(usize, usize)>) {
        let a = Match::new(a.0, a.1);
        let b = Match::new(b.0, b.1);
        let res = res.map(|(start, end)| Match::new(start, end));
        assert_eq!(a.merge(&b), res);
        assert_eq!(b.merge(&a), res);
    }
}
