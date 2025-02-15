use crate::regex::error::SyntaxError;
use crate::regex::parser::Parser;
use std::str::FromStr;

pub mod error;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Regex {
    pub(crate) expression: Expression,
    pub(crate) start_anchor: bool,
    pub(crate) end_anchor: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Expression {
    Term(Term),
    Alternation(Term, Box<Expression>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Term {
    Factor(Factor),
    Concatenation(Factor, Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Factor {
    Atom(Atom),
    ZeroOrOne(Atom),
    ZeroOrMore(Atom),
    OneOrMore(Atom),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Atom {
    Char(char),
    AnyChar,
    Parentheses(Box<Expression>),
    NormalClass(CharClass),
    NegatedClass(CharClass),
    BackReference(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CharClass {
    CharRange(CharRange),
    CharRangeClass(CharRange, Box<CharClass>),
    Alphanumeric,
    Digit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CharRange {
    Char(char),
    CharRange(char, char),
}

impl FromStr for Regex {
    type Err = SyntaxError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Parser::new(s).parse()
    }
}
