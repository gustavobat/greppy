use crate::regex::error::SyntaxError;
use crate::regex::lexer::Lexer;
use crate::regex::lexer::TokenKind;
use crate::regex::Atom;
use crate::regex::CharClass;
use crate::regex::CharRange;
use crate::regex::Expression;
use crate::regex::Factor;
use crate::regex::Regex;
use crate::regex::Term;

pub(crate) struct Parser<'s> {
    lexer: Lexer<'s>,
}

impl Parser<'_> {
    pub fn new(input: &str) -> Parser {
        Parser {
            lexer: Lexer::new(input),
        }
    }

    pub fn parse(&mut self) -> Result<Regex, SyntaxError> {
        let mut start_anchor = false;
        if let Some(Ok(token)) = self.lexer.peek() {
            if token.kind == TokenKind::Caret {
                start_anchor = true;
                self.lexer.next();
            }
        }
        let expression = self.parse_expression()?;
        let mut end_anchor = false;
        if let Some(Ok(token)) = self.lexer.peek() {
            if token.kind == TokenKind::DollarSign {
                end_anchor = true;
                self.lexer.next();
            }
        }
        Ok(Regex {
            expression,
            start_anchor,
            end_anchor,
        })
    }

    fn parse_single_char(&mut self) -> Result<char, SyntaxError> {
        match self.lexer.next() {
            Some(Ok(token)) => match token.kind {
                TokenKind::Char(c) => Ok(c),
                _ => Err(SyntaxError::Unexpected("'Char'".to_string(), token.kind)),
            },
            _ => Err(SyntaxError::UnexpectedEOF),
        }
    }

    fn parse_char_range(&mut self) -> Result<CharRange, SyntaxError> {
        let start = self.parse_single_char()?;
        if let Some(Ok(token)) = self.lexer.peek() {
            if token.kind == TokenKind::Minus {
                self.lexer.next();
                let end = self.parse_single_char()?;
                return Ok(CharRange::CharRange(start, end));
            }
        }
        Ok(CharRange::Char(start))
    }

    fn parse_char_class(&mut self) -> Result<CharClass, SyntaxError> {
        if let Some(Ok(token)) = self.lexer.peek() {
            if token.kind == TokenKind::Escaped('w') {
                self.lexer.next();
                return Ok(CharClass::Alphanumeric);
            } else if token.kind == TokenKind::Escaped('d') {
                self.lexer.next();
                return Ok(CharClass::Digit);
            }
        }
        let char_range = self.parse_char_range()?;
        if let Some(Ok(token)) = self.lexer.peek() {
            if token.kind != TokenKind::RightBracket {
                let char_class = self.parse_char_class()?;
                return Ok(CharClass::CharRangeClass(char_range, Box::new(char_class)));
            }
        }
        Ok(CharClass::CharRange(char_range))
    }

    fn parse_atom(&mut self) -> Result<Atom, SyntaxError> {
        match self.lexer.peek() {
            Some(Ok(token)) => match token.kind {
                TokenKind::Char(c) => {
                    self.lexer.next();
                    Ok(Atom::Char(c))
                }
                TokenKind::Dot => {
                    self.lexer.next();
                    Ok(Atom::AnyChar)
                }
                TokenKind::LeftParen => {
                    self.lexer.next();
                    let expr = self.parse_expression()?;
                    match self.lexer.next() {
                        Some(Ok(token)) => {
                            if token.kind == TokenKind::RightParen {
                                Ok(Atom::Parentheses(Box::new(expr)))
                            } else {
                                Err(SyntaxError::Unexpected("')'".to_string(), token.kind))
                            }
                        }
                        None => Err(SyntaxError::UnexpectedEOF),
                        Some(Err(token_error)) => Err(SyntaxError::InvalidToken(token_error)),
                    }
                }
                TokenKind::LeftBracket => {
                    self.lexer.next();
                    let mut negated = false;
                    if let Some(Ok(token)) = self.lexer.peek() {
                        if token.kind == TokenKind::Caret {
                            self.lexer.next();
                            negated = true;
                        }
                    }
                    let char_class = self.parse_char_class()?;
                    match self.lexer.next() {
                        Some(Ok(token)) => {
                            if token.kind == TokenKind::RightBracket {
                                if negated {
                                    return Ok(Atom::NegatedClass(char_class));
                                }
                                Ok(Atom::NormalClass(char_class))
                            } else {
                                Err(SyntaxError::Unexpected("']'".to_string(), token.kind))
                            }
                        }
                        None => Err(SyntaxError::UnexpectedEOF),
                        Some(Err(token_error)) => Err(SyntaxError::InvalidToken(token_error)),
                    }
                }
                TokenKind::Escaped(c) => {
                    if c.is_numeric() {
                        self.lexer.next();
                        return Ok(Atom::BackReference(c.to_digit(10).unwrap() as usize));
                    };
                    let char_class = self.parse_char_class()?;
                    Ok(Atom::NormalClass(char_class))
                }
                _ => Err(SyntaxError::InvalidAtomStart),
            },
            None => Err(SyntaxError::UnexpectedEOF),
            Some(Err(token_error)) => Err(SyntaxError::InvalidToken(token_error.clone())),
        }
    }

    fn parse_factor(&mut self) -> Result<Factor, SyntaxError> {
        let atom = self.parse_atom()?;
        match self.lexer.peek() {
            Some(Ok(token)) => match token.kind {
                TokenKind::QuestionMark => {
                    self.lexer.next();
                    Ok(Factor::ZeroOrOne(atom))
                }
                TokenKind::Star => {
                    self.lexer.next();
                    Ok(Factor::ZeroOrMore(atom))
                }
                TokenKind::Plus => {
                    self.lexer.next();
                    Ok(Factor::OneOrMore(atom))
                }
                _ => Ok(Factor::Atom(atom)),
            },
            None => Ok(Factor::Atom(atom)),
            Some(Err(token_error)) => Err(SyntaxError::InvalidToken(token_error.clone())),
        }
    }
    fn parse_term(&mut self) -> Result<Term, SyntaxError> {
        let factor = self.parse_factor()?;
        match self.lexer.peek() {
            Some(Ok(token)) => match token.kind {
                TokenKind::Char(_)
                | TokenKind::Dot
                | TokenKind::LeftParen
                | TokenKind::Escaped(_)
                | TokenKind::LeftBracket => {
                    let term = self.parse_term()?;
                    Ok(Term::Concatenation(factor, Box::new(term)))
                }
                _ => Ok(Term::Factor(factor)),
            },
            _ => Ok(Term::Factor(factor)),
        }
    }

    pub(crate) fn parse_expression(&mut self) -> Result<Expression, SyntaxError> {
        let term = self.parse_term()?;
        match self.lexer.peek() {
            Some(Ok(token)) => {
                if token.kind == TokenKind::Pipe {
                    self.lexer.next();
                    let expr = self.parse_expression()?;
                    return Ok(Expression::Alternation(term, Box::new(expr)));
                }
            }
            _ => return Ok(Expression::Term(term)),
        }
        Ok(Expression::Term(term))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::regex::CharRange;

    #[test]
    fn test_parse_char() {
        let mut parser = Parser::new("a");
        assert_eq!(parser.parse_single_char(), Ok('a'));
    }

    #[test]
    fn test_parse_char_range() {
        let mut parser = Parser::new("a-b");
        assert_eq!(
            parser.parse_char_range(),
            Ok(CharRange::CharRange('a', 'b'))
        );
        let mut parser = Parser::new("a");
        assert_eq!(parser.parse_char_range(), Ok(CharRange::Char('a')));
    }

    #[test]
    fn test_parse_char_class() {
        let mut parser = Parser::new("a-b");
        assert_eq!(
            parser.parse_char_class(),
            Ok(CharClass::CharRange(CharRange::CharRange('a', 'b')))
        );

        let mut parser = Parser::new("a-bc-d");
        assert_eq!(
            parser.parse_char_class(),
            Ok(CharClass::CharRangeClass(
                CharRange::CharRange('a', 'b'),
                Box::new(CharClass::CharRange(CharRange::CharRange('c', 'd')))
            ))
        );

        let mut parser = Parser::new("\\w");
        assert_eq!(parser.parse_char_class(), Ok(CharClass::Alphanumeric));

        let mut parser = Parser::new("\\d");
        assert_eq!(parser.parse_char_class(), Ok(CharClass::Digit));
    }

    #[test]
    fn test_parse_atom() {
        let mut parser = Parser::new("a");
        assert_eq!(parser.parse_atom(), Ok(Atom::Char('a')));

        let mut parser = Parser::new(".");
        assert_eq!(parser.parse_atom(), Ok(Atom::AnyChar));

        let mut parser = Parser::new("(a)");
        assert_eq!(
            parser.parse_atom(),
            Ok(Atom::Parentheses(Box::new(Expression::Term(Term::Factor(
                Factor::Atom(Atom::Char('a'))
            )))))
        );

        let mut parser = Parser::new("\\1");
        assert_eq!(parser.parse_atom(), Ok(Atom::BackReference(1)));
    }

    #[test]
    fn test_parse_factor() {
        let mut parser = Parser::new("a?");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor::ZeroOrOne(Atom::Char('a')))
        );

        let mut parser = Parser::new("a*");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor::ZeroOrMore(Atom::Char('a')))
        );

        let mut parser = Parser::new("a+");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor::OneOrMore(Atom::Char('a')))
        );

        let mut parser = Parser::new("a");
        assert_eq!(parser.parse_factor(), Ok(Factor::Atom(Atom::Char('a'))));

        let mut parser = Parser::new("[abc]");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor::Atom(Atom::NormalClass(CharClass::CharRangeClass(
                CharRange::Char('a'),
                Box::new(CharClass::CharRangeClass(
                    CharRange::Char('b'),
                    Box::new(CharClass::CharRange(CharRange::Char('c')))
                ))
            ))))
        );
    }

    #[test]
    fn test_parse_term() {
        let mut parser = Parser::new("a.b");
        assert_eq!(
            parser.parse_term(),
            Ok(Term::Concatenation(
                Factor::Atom(Atom::Char('a')),
                Box::new(Term::Concatenation(
                    Factor::Atom(Atom::AnyChar),
                    Box::new(Term::Factor(Factor::Atom(Atom::Char('b'))))
                ))
            ))
        );

        let mut parser = Parser::new("a");
        assert_eq!(
            parser.parse_term(),
            Ok(Term::Factor(Factor::Atom(Atom::Char('a'))))
        );
    }

    #[test]
    fn test_parse_expression() {
        let mut parser = Parser::new("a");
        assert_eq!(
            parser.parse_expression(),
            Ok(Expression::Term(Term::Factor(Factor::Atom(Atom::Char(
                'a'
            )))))
        );

        let mut parser = Parser::new("a|b");
        assert_eq!(
            parser.parse_expression(),
            Ok(Expression::Alternation(
                Term::Factor(Factor::Atom(Atom::Char('a'))),
                Box::new(Expression::Term(Term::Factor(Factor::Atom(Atom::Char(
                    'b'
                )))))
            ))
        );
    }

    #[test]
    fn test_parse() {
        let mut parser = Parser::new("a");
        assert_eq!(
            parser.parse(),
            Ok(Regex {
                expression: Expression::Term(Term::Factor(Factor::Atom(Atom::Char('a')))),
                start_anchor: false,
                end_anchor: false
            })
        );

        let mut parser = Parser::new("^a$");
        assert_eq!(
            parser.parse(),
            Ok(Regex {
                expression: Expression::Term(Term::Factor(Factor::Atom(Atom::Char('a')))),
                start_anchor: true,
                end_anchor: true
            })
        );
    }
}
