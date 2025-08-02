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
pub enum Factor {
    Atom(Atom),
    ZeroOrOne(Atom),
    ZeroOrMore(Atom),
    OneOrMore(Atom),
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
