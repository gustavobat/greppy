use std::str::FromStr;

use crate::error::ExpressionError;

use nom::bytes::complete::tag;
use nom::multi::many1;
use nom::IResult;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Tag(String),
    Digit,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Expression {
    pub tokens: Vec<Token>,
}

impl Expression {
    pub fn push(&mut self, token: Token) {
        self.tokens.push(token);
    }
}

fn parse_digit(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("\\d")(input)?;
    Ok((input, Token::Digit))
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
    let (rest, patterns) = many1(nom::branch::alt((parse_tag, parse_digit)))(input)
        .map_err(|_| ExpressionError::Unsupported(input.to_owned()))?;
    if !rest.is_empty() {
        return Err(ExpressionError::Unsupported(rest.to_owned()));
    }
    Ok(Expression { tokens: patterns })
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
    #[test_case("12", vec![Token::Digit, Token::Digit]; "multiple digits")]
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
        let result = Expression::from_str("a$").unwrap_err();
        assert_eq!(result, ExpressionError::Unsupported("$".to_string()));
    }
}
