mod lexer;
mod parser;

use parser::Parser;
use parser::SyntaxError;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Regex {
    pub expression: Expression,
    pub start_anchor: bool,
    pub end_anchor: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Term(Term),
    Alternation(Term, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Factor(Factor),
    Concatenation(Factor, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpperBound {
    Exactly(usize),
    Unbounded,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Factor {
    pub atom: Atom,
    pub min: usize,
    pub max: UpperBound,
}

impl Factor {
    /// Creates a new Factor, ensuring that max >= min (when max is bounded)
    pub fn new(atom: Atom, min: usize, max: UpperBound) -> Self {
        // Validate invariant: if max is Exactly(n), then n >= min
        if let UpperBound::Exactly(max_val) = max {
            debug_assert!(
                max_val >= min,
                "Factor max ({}) must be >= min ({})",
                max_val,
                min
            );
        }
        Self { atom, min, max }
    }

    /// Returns true if this Factor has valid bounds
    pub fn is_valid(&self) -> bool {
        match self.max {
            UpperBound::Exactly(max_val) => max_val >= self.min,
            UpperBound::Unbounded => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Atom {
    Char(char),
    Parentheses { id: usize, expr: Box<Expression> },
    NormalClass(CharClass),
    NegatedClass(CharClass),
    BackReference(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CharClass {
    CharRange(CharRange),
    CharRangeClass(CharRange, Box<CharClass>),
    WordChar,
    Digit,
    AnyChar,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CharRange {
    Char(char),
    CharRange(char, char),
}

impl FromStr for Regex {
    type Err = SyntaxError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Parser::new(s).parse()
    }
}
