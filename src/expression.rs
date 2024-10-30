use std::collections::HashSet;
use std::str::FromStr;

use crate::error::ExpressionError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Tag(char),
    Digit,
    AlphaNumeric,
    PosCharGroup(HashSet<char>),
    NegCharGroup(HashSet<char>),
    OneOrMore(char),
    ZeroOrMore(char),
    Wildcard,
    Alternation((Expression, Expression)),
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Expression {
    pub tokens: Vec<Token>,
    pub start_anchor: bool,
    pub end_anchor: bool,
}

fn parse_expression(input: &str) -> Result<Expression, ExpressionError> {
    if input.is_empty() {
        return Err(ExpressionError::EmptyExpression);
    }

    let mut expression = Expression::default();
    let mut tokens = vec![];

    let mut expr_str = input;
    if let Some(stripped) = input.strip_prefix("^") {
        expr_str = stripped;
        expression.start_anchor = true;
    }

    if let Some(stripped) = expr_str.strip_suffix("$") {
        expr_str = stripped;
        expression.end_anchor = true;
    }

    let mut iter = expr_str.chars().peekable();

    while let Some(c) = iter.next() {
        match c {
            '\\' => {
                let next = iter.next();
                match next {
                    Some('d') => tokens.push(Token::Digit),
                    Some('w') => tokens.push(Token::AlphaNumeric),
                    Some(c) => return Err(ExpressionError::Unsupported(format!("\\{}", c))),
                    None => return Err(ExpressionError::Unsupported("\\".to_string())),
                }
            }
            '[' => {
                let mut group = HashSet::new();
                let mut negated = false;

                if let Some('^') = iter.peek() {
                    negated = true;
                    iter.next();
                }

                for c in iter.by_ref() {
                    match c {
                        ']' => break,
                        c => {
                            group.insert(c);
                        }
                    }
                }

                if negated {
                    tokens.push(Token::NegCharGroup(group));
                } else {
                    tokens.push(Token::PosCharGroup(group));
                }
            }
            '+' => {
                let last_token = tokens
                    .pop()
                    .ok_or(ExpressionError::QualifierNotPreceded('+'))?;
                match last_token {
                    Token::Tag(c) => tokens.push(Token::OneOrMore(c)),
                    _ => return Err(ExpressionError::Unsupported("+".to_string())),
                }
            }
            '?' => {
                let last_token = tokens
                    .pop()
                    .ok_or(ExpressionError::QualifierNotPreceded('?'))?;
                match last_token {
                    Token::Tag(c) => tokens.push(Token::ZeroOrMore(c)),
                    _ => return Err(ExpressionError::Unsupported("?".to_string())),
                }
            }
            '.' => tokens.push(Token::Wildcard),
            '(' => {
                let mut left = vec![];
                let mut right = vec![];

                let mut current = &mut left;
                for c in iter.by_ref() {
                    match c {
                        '|' => {
                            current = &mut right;
                        }
                        ')' => break,
                        c => current.push(Token::Tag(c)),
                    }
                }

                tokens.push(Token::Alternation((left.into(), right.into())));
            }
            '^' | '$' => return Err(ExpressionError::InvalidAnchorPosition(c)),
            c => tokens.push(Token::Tag(c)),
        }
    }

    expression.tokens = tokens;
    Ok(expression)
}

impl FromStr for Expression {
    type Err = ExpressionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_expression(s)
    }
}

impl From<Vec<Token>> for Expression {
    fn from(tokens: Vec<Token>) -> Self {
        Expression {
            tokens,
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use test_case::test_case;

    #[test_case("a", vec![Token::Tag('a')]; "single char")]
    #[test_case("ab", vec![Token::Tag('a'), Token::Tag('b')]; "multiple chars")]
    #[test_case("12", vec![Token::Tag('1'), Token::Tag('2')]; "numeric chars")]
    #[test_case("\\d", vec![Token::Digit]; "digit token")]
    #[test_case("\\w", vec![Token::AlphaNumeric]; "alphanumeric token")]
    #[test_case("[ab]", vec![Token::PosCharGroup(HashSet::from(['a', 'b']))]; "positive char group")]
    #[test_case("[^ab]", vec![Token::NegCharGroup(HashSet::from(['a', 'b']))]; "negative char group")]
    #[test_case("ab+", vec![Token::Tag('a'), Token::OneOrMore('b')]; "one ore more")]
    #[test_case("ab?", vec![Token::Tag('a'), Token::ZeroOrMore('b')]; "zero or more")]
    #[test_case("a.", vec![Token::Tag('a'), Token::Wildcard]; "wildcard")]
    fn test_expression(input: &str, expected_tokens: Vec<Token>) {
        let result = Expression::from_str(input).unwrap();
        assert_eq!(result.tokens, expected_tokens);
    }

    #[test]
    fn test_alternation() {
        let result = Expression::from_str("(a|b)").unwrap();
        assert_eq!(
            result.tokens,
            vec![Token::Alternation((
                vec![Token::Tag('a')].into(),
                vec![Token::Tag('b')].into()
            ))]
        );
    }

    #[test]
    fn test_empty_expression() {
        let result = Expression::from_str("").unwrap_err();
        assert_eq!(result, ExpressionError::EmptyExpression);
    }

    #[test]
    fn test_unsupported_token() {
        let result = Expression::from_str("a\\n").unwrap_err();
        assert_eq!(result, ExpressionError::Unsupported("\\n".to_string()));
    }

    #[test]
    fn test_anchors() {
        let result = Expression::from_str("^a").unwrap();
        assert!(result.start_anchor);

        let result = Expression::from_str("a$").unwrap();
        assert!(result.end_anchor);
    }

    #[test]
    fn test_anchors_misplacement() {
        let result = Expression::from_str("a^").unwrap_err();
        assert_eq!(result, ExpressionError::InvalidAnchorPosition('^'));

        let result = Expression::from_str("$a").unwrap_err();
        assert_eq!(result, ExpressionError::InvalidAnchorPosition('$'));
    }
}
