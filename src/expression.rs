use std::collections::HashSet;
use std::str::FromStr;

use crate::error::ExpressionError;

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_until;
use nom::multi::many1;
use nom::sequence::tuple;
use nom::IResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Tag(String),
    Digit,
    AlphaNumeric,
    PosCharGroup(HashSet<char>),
    NegCharGroup(HashSet<char>),
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Expression {
    pub tokens: Vec<Token>,
    pub start_anchor: bool,
    pub end_anchor: bool,
}

fn parse_digit(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("\\d")(input)?;
    Ok((input, Token::Digit))
}

fn parse_alphanumeric(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("\\w")(input)?;
    Ok((input, Token::AlphaNumeric))
}

fn parse_char_group(input: &str) -> IResult<&str, Token> {
    let (input, (_, chars, _)) = tuple((tag("["), take_until("]"), tag("]")))(input)?;
    if let Some(chars) = chars.strip_prefix("^") {
        return Ok((input, Token::NegCharGroup(chars.chars().collect())));
    }
    Ok((input, Token::PosCharGroup(chars.chars().collect())))
}

fn parse_tag(input: &str) -> IResult<&str, Token> {
    let (input, tag) = many1(nom::branch::alt((
        nom::character::complete::alphanumeric1,
        nom::character::complete::space1,
    )))(input)?;
    Ok((input, Token::Tag(tag.into_iter().collect())))
}

fn parse_expression(input: &str) -> Result<Expression, ExpressionError> {
    if input.is_empty() {
        return Err(ExpressionError::EmptyExpression);
    }

    let mut expression = Expression::default();

    let mut expression_str = input;
    if input.starts_with("^") {
        expression.start_anchor = true;
        expression_str = &expression_str[1..];
    }

    if input.ends_with("$") {
        expression.end_anchor = true;
        expression_str = &expression_str[..expression_str.len() - 1];
    }

    let (rest, tokens) = many1(alt((
        parse_digit,
        parse_alphanumeric,
        parse_char_group,
        parse_tag,
    )))(expression_str)
    .map_err(|_| ExpressionError::Unsupported(input.to_owned()))?;

    if !rest.is_empty() {
        return Err(ExpressionError::Unsupported(rest.to_owned()))?;
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

#[cfg(test)]
mod tests {
    use super::*;

    use test_case::test_case;

    #[test_case("a", vec![Token::Tag("a".to_owned())]; "single char")]
    #[test_case("ab", vec![Token::Tag("ab".to_owned())]; "multiple chars")]
    #[test_case("12", vec![Token::Tag("12".to_owned())]; "numeric tag")]
    #[test_case("\\d", vec![Token::Digit]; "digit token")]
    #[test_case("\\w", vec![Token::AlphaNumeric]; "alphanumeric token")]
    #[test_case("[ab]", vec![Token::PosCharGroup(HashSet::from(['a', 'b']))]; "positive char group")]
    #[test_case("[^ab]", vec![Token::NegCharGroup(HashSet::from(['a', 'b']))]; "negative char group")]
    fn test_expression(input: &str, expected_tokens: Vec<Token>) {
        let result = Expression::from_str(input).unwrap();
        assert_eq!(result.tokens, expected_tokens);
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
        assert_eq!(result, ExpressionError::Unsupported("^".to_string()));

        let result = Expression::from_str("$a").unwrap_err();
        assert_eq!(result, ExpressionError::Unsupported("$a".to_string()));
    }
}
