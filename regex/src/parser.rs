use crate::Atom;
use crate::CharClass;
use crate::CharRange;
use crate::Expression;
use crate::Factor;
use crate::Regex;
use crate::Term;
use crate::UpperBound;
use crate::lexer::Lexer;
use crate::lexer::TokenError;
use crate::lexer::TokenKind;
use thiserror::Error;

#[derive(Debug, Clone)]
pub(crate) struct Parser<'s> {
    lexer: Lexer<'s>,
    capturing_group_counter: usize,
}

#[derive(Error, Clone, Debug, PartialEq, Eq)]
pub enum SyntaxError {
    #[error("Invalid token {0:?}")]
    InvalidToken(TokenError),
    #[error("Expected {0} got {1:?}")]
    Unexpected(String, TokenKind),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Invalid atom start")]
    InvalidAtomStart,
    #[error("Backreference {0} used before capturing group was defined")]
    BackreferenceUsedBeforeCapture(usize),
    #[error("Invalid quantifier range: max ({0}) must be >= min ({1})")]
    InvalidQuantifierRange(usize, usize),
    #[error("Invalid number format in range definition")]
    InvalidNumberFormatInRangeDefinition,
}

impl Parser<'_> {
    pub fn new(input: &str) -> Parser<'_> {
        Parser {
            lexer: Lexer::new(input),
            capturing_group_counter: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Regex, SyntaxError> {
        let mut start_anchor = false;
        if let Some(Ok(token)) = self.lexer.peek()
            && token.kind == TokenKind::Caret
        {
            start_anchor = true;
            self.lexer.next();
        }
        let expression = self.parse_expression()?;
        let mut end_anchor = false;
        if let Some(Ok(token)) = self.lexer.peek()
            && token.kind == TokenKind::DollarSign
        {
            end_anchor = true;
            self.lexer.next();
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
        if let Some(Ok(token)) = self.lexer.peek()
            && token.kind == TokenKind::Minus
        {
            self.lexer.next();
            let end = self.parse_single_char()?;
            return Ok(CharRange::CharRange(start, end));
        }
        Ok(CharRange::Char(start))
    }

    fn parse_char_class(&mut self) -> Result<CharClass, SyntaxError> {
        if let Some(Ok(token)) = self.lexer.peek() {
            if token.kind == TokenKind::Escaped('w') {
                self.lexer.next();
                return Ok(CharClass::WordChar);
            } else if token.kind == TokenKind::Escaped('d') {
                self.lexer.next();
                return Ok(CharClass::Digit);
            } else if token.kind == TokenKind::Dot {
                self.lexer.next();
                return Ok(CharClass::AnyChar);
            }
        }
        let char_range = self.parse_char_range()?;
        if let Some(Ok(token)) = self.lexer.peek()
            && token.kind != TokenKind::RightBracket
        {
            let char_class = self.parse_char_class()?;
            return Ok(CharClass::CharRangeClass(char_range, Box::new(char_class)));
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
                    let char_class = self.parse_char_class()?;
                    Ok(Atom::NormalClass(char_class))
                }
                TokenKind::Minus => {
                    self.lexer.next();
                    Ok(Atom::Char('-'))
                }
                TokenKind::LeftParen => {
                    self.lexer.next();
                    // backreferences are1-based, so the increment is done before the assignment
                    self.capturing_group_counter += 1;
                    let id = self.capturing_group_counter;
                    let expr = self.parse_expression()?;
                    match self.lexer.next() {
                        Some(Ok(token)) => {
                            if token.kind == TokenKind::RightParen {
                                Ok(Atom::Parentheses {
                                    id,
                                    expr: Box::new(expr),
                                })
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
                    if let Some(Ok(token)) = self.lexer.peek()
                        && token.kind == TokenKind::Caret
                    {
                        self.lexer.next();
                        negated = true;
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
                        let n = c.to_digit(10).unwrap() as usize;
                        if n > self.capturing_group_counter {
                            return Err(SyntaxError::BackreferenceUsedBeforeCapture(n));
                        }
                        return Ok(Atom::BackReference(n));
                    };

                    if c == 'n' {
                        self.lexer.next();
                        return Ok(Atom::Char('\n'));
                    } else if c == 'r' {
                        self.lexer.next();
                        return Ok(Atom::Char('\r'));
                    } else if c == 't' {
                        self.lexer.next();
                        return Ok(Atom::Char('\t'));
                    }
                    if matches!(c, '[' | ']' | '\\' | '(' | ')') {
                        self.lexer.next();
                        return Ok(Atom::Char(c));
                    }
                    let char_class = self.parse_char_class()?;
                    Ok(Atom::NormalClass(char_class))
                }
                _ => Err(SyntaxError::InvalidAtomStart),
            },
            None => Err(SyntaxError::UnexpectedEOF),
            Some(Err(token_error)) => Err(SyntaxError::InvalidToken(token_error.clone())),
        }
    }

    fn parse_number(&mut self) -> Result<usize, SyntaxError> {
        let mut digits = String::new();

        while let Some(Ok(token)) = self.lexer.peek() {
            if let TokenKind::Char(c) = token.kind {
                if c.is_ascii_digit() {
                    digits.push(c);
                    self.lexer.next();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if digits.is_empty() {
            return Err(SyntaxError::InvalidNumberFormatInRangeDefinition);
        }

        digits
            .parse::<usize>()
            .map_err(|_| SyntaxError::InvalidNumberFormatInRangeDefinition)
    }

    fn parse_factor(&mut self) -> Result<Factor, SyntaxError> {
        let atom = self.parse_atom()?;
        match self.lexer.peek() {
            Some(Ok(token)) => match token.kind {
                TokenKind::QuestionMark => {
                    self.lexer.next();
                    Ok(Factor {
                        atom,
                        min: 0,
                        max: UpperBound::Exactly(1),
                    })
                }
                TokenKind::Star => {
                    self.lexer.next();
                    Ok(Factor {
                        atom,
                        min: 0,
                        max: UpperBound::Unbounded,
                    })
                }
                TokenKind::Plus => {
                    self.lexer.next();
                    Ok(Factor {
                        atom,
                        min: 1,
                        max: UpperBound::Unbounded,
                    })
                }
                TokenKind::LeftBrace => {
                    self.lexer.next();
                    let min = self.parse_number()?;

                    let max = if let Some(Ok(token)) = self.lexer.peek() {
                        if let TokenKind::Char(',') = token.kind {
                            self.lexer.next();

                            if let Some(Ok(token)) = self.lexer.peek() {
                                if token.kind == TokenKind::RightBrace {
                                    UpperBound::Unbounded
                                } else {
                                    let max_val = self.parse_number()?;
                                    if max_val < min {
                                        return Err(SyntaxError::InvalidQuantifierRange(
                                            max_val, min,
                                        ));
                                    }
                                    UpperBound::Exactly(max_val)
                                }
                            } else {
                                return Err(SyntaxError::UnexpectedEOF);
                            }
                        } else {
                            UpperBound::Exactly(min)
                        }
                    } else {
                        return Err(SyntaxError::UnexpectedEOF);
                    };

                    match self.lexer.next() {
                        Some(Ok(token)) if token.kind == TokenKind::RightBrace => {
                            Ok(Factor { atom, min, max })
                        }
                        Some(Ok(token)) => {
                            Err(SyntaxError::Unexpected("'}'".to_string(), token.kind))
                        }
                        Some(Err(e)) => Err(SyntaxError::InvalidToken(e)),
                        None => Err(SyntaxError::UnexpectedEOF),
                    }
                }
                _ => Ok(Factor {
                    atom,
                    min: 1,
                    max: UpperBound::Exactly(1),
                }),
            },
            None => Ok(Factor {
                atom,
                min: 1,
                max: UpperBound::Exactly(1),
            }),
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
                | TokenKind::Minus
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

        let mut parser = Parser::new(".");
        assert_eq!(parser.parse_char_class(), Ok(CharClass::AnyChar));

        let mut parser = Parser::new("a-bc-d");
        assert_eq!(
            parser.parse_char_class(),
            Ok(CharClass::CharRangeClass(
                CharRange::CharRange('a', 'b'),
                Box::new(CharClass::CharRange(CharRange::CharRange('c', 'd')))
            ))
        );

        let mut parser = Parser::new("\\w");
        assert_eq!(parser.parse_char_class(), Ok(CharClass::WordChar));

        let mut parser = Parser::new("\\d");
        assert_eq!(parser.parse_char_class(), Ok(CharClass::Digit));
    }

    #[test]
    fn test_parse_atom() {
        let mut parser = Parser::new("a");
        assert_eq!(parser.parse_atom(), Ok(Atom::Char('a')));

        let mut parser = Parser::new("-");
        assert_eq!(parser.parse_atom(), Ok(Atom::Char('-')));

        let mut parser = Parser::new("(a)");
        assert_eq!(
            parser.parse_atom(),
            Ok(Atom::Parentheses {
                id: 1,
                expr: Box::new(Expression::Term(Term::Factor(Factor {
                    atom: Atom::Char('a'),
                    min: 1,
                    max: UpperBound::Exactly(1),
                })))
            })
        );
    }

    #[test]
    fn test_parse_factor() {
        let mut parser = Parser::new("a?");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::Char('a'),
                min: 0,
                max: UpperBound::Exactly(1),
            })
        );

        let mut parser = Parser::new("a*");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::Char('a'),
                min: 0,
                max: UpperBound::Unbounded,
            })
        );

        let mut parser = Parser::new("a+");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::Char('a'),
                min: 1,
                max: UpperBound::Unbounded,
            })
        );

        let mut parser = Parser::new("a");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::Char('a'),
                min: 1,
                max: UpperBound::Exactly(1),
            })
        );

        let mut parser = Parser::new("[abc]");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::NormalClass(CharClass::CharRangeClass(
                    CharRange::Char('a'),
                    Box::new(CharClass::CharRangeClass(
                        CharRange::Char('b'),
                        Box::new(CharClass::CharRange(CharRange::Char('c')))
                    ))
                )),
                min: 1,
                max: UpperBound::Exactly(1),
            })
        );
    }

    #[test]
    fn test_parse_term() {
        let mut parser = Parser::new("a.b");
        assert_eq!(
            parser.parse_term(),
            Ok(Term::Concatenation(
                Factor {
                    atom: Atom::Char('a'),
                    min: 1,
                    max: UpperBound::Exactly(1),
                },
                Box::new(Term::Concatenation(
                    Factor {
                        atom: Atom::NormalClass(CharClass::AnyChar),
                        min: 1,
                        max: UpperBound::Exactly(1),
                    },
                    Box::new(Term::Factor(Factor {
                        atom: Atom::Char('b'),
                        min: 1,
                        max: UpperBound::Exactly(1),
                    }))
                ))
            ))
        );

        let mut parser = Parser::new("a");
        assert_eq!(
            parser.parse_term(),
            Ok(Term::Factor(Factor {
                atom: Atom::Char('a'),
                min: 1,
                max: UpperBound::Exactly(1),
            }))
        );
    }

    #[test]
    fn test_parse_expression() {
        let mut parser = Parser::new("a");
        assert_eq!(
            parser.parse_expression(),
            Ok(Expression::Term(Term::Factor(Factor {
                atom: Atom::Char('a'),
                min: 1,
                max: UpperBound::Exactly(1),
            })))
        );

        let mut parser = Parser::new("a|b");
        assert_eq!(
            parser.parse_expression(),
            Ok(Expression::Alternation(
                Term::Factor(Factor {
                    atom: Atom::Char('a'),
                    min: 1,
                    max: UpperBound::Exactly(1),
                }),
                Box::new(Expression::Term(Term::Factor(Factor {
                    atom: Atom::Char('b'),
                    min: 1,
                    max: UpperBound::Exactly(1),
                })))
            ))
        );
    }

    #[test]
    fn test_parse_anchors() {
        let mut parser = Parser::new("a");
        assert_eq!(
            parser.parse(),
            Ok(Regex {
                expression: Expression::Term(Term::Factor(Factor {
                    atom: Atom::Char('a'),
                    min: 1,
                    max: UpperBound::Exactly(1),
                })),
                start_anchor: false,
                end_anchor: false
            })
        );

        let mut parser = Parser::new("^a$");
        assert_eq!(
            parser.parse(),
            Ok(Regex {
                expression: Expression::Term(Term::Factor(Factor {
                    atom: Atom::Char('a'),
                    min: 1,
                    max: UpperBound::Exactly(1),
                })),
                start_anchor: true,
                end_anchor: true
            })
        );
    }

    #[test]
    fn test_parse_escaped_chars() {
        let mut parser = Parser::new(r"\d\n\\a");
        assert_eq!(
            parser.parse(),
            Ok(Regex {
                expression: Expression::Term(Term::Concatenation(
                    Factor {
                        atom: Atom::NormalClass(CharClass::Digit),
                        min: 1,
                        max: UpperBound::Exactly(1),
                    },
                    Box::new(Term::Concatenation(
                        Factor {
                            atom: Atom::Char('\n'),
                            min: 1,
                            max: UpperBound::Exactly(1),
                        },
                        Box::new(Term::Concatenation(
                            Factor {
                                atom: Atom::Char('\\'),
                                min: 1,
                                max: UpperBound::Exactly(1),
                            },
                            Box::new(Term::Factor(Factor {
                                atom: Atom::Char('a'),
                                min: 1,
                                max: UpperBound::Exactly(1),
                            }))
                        ))
                    ))
                )),
                start_anchor: false,
                end_anchor: false
            })
        );
    }

    #[test]
    fn backreference_before_capture() {
        let mut parser = Parser::new(r"\1");
        assert_eq!(
            parser.parse(),
            Err(SyntaxError::BackreferenceUsedBeforeCapture(1))
        );

        let mut parser = Parser::new(r"(a)\1");
        assert_eq!(
            parser.parse(),
            Ok(Regex {
                expression: Expression::Term(Term::Concatenation(
                    Factor {
                        atom: Atom::Parentheses {
                            id: 1,
                            expr: Box::new(Expression::Term(Term::Factor(Factor {
                                atom: Atom::Char('a'),
                                min: 1,
                                max: UpperBound::Exactly(1),
                            })))
                        },
                        min: 1,
                        max: UpperBound::Exactly(1),
                    },
                    Box::new(Term::Factor(Factor {
                        atom: Atom::BackReference(1),
                        min: 1,
                        max: UpperBound::Exactly(1),
                    }))
                )),
                start_anchor: false,
                end_anchor: false
            })
        );

        let mut parser = Parser::new(r"(a)\2");
        assert_eq!(
            parser.parse(),
            Err(SyntaxError::BackreferenceUsedBeforeCapture(2))
        );
    }

    #[test]
    fn test_parse_quantifier_exact() {
        let mut parser = Parser::new("a{3}");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::Char('a'),
                min: 3,
                max: UpperBound::Exactly(3),
            })
        );
    }

    #[test]
    fn test_parse_quantifier_at_least() {
        let mut parser = Parser::new("a{2,}");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::Char('a'),
                min: 2,
                max: UpperBound::Unbounded,
            })
        );
    }

    #[test]
    fn test_parse_quantifier_range() {
        let mut parser = Parser::new("a{1,5}");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::Char('a'),
                min: 1,
                max: UpperBound::Exactly(5),
            })
        );
    }

    #[test]
    fn test_parse_quantifier_invalid_range() {
        let mut parser = Parser::new("a{5,2}");
        assert_eq!(
            parser.parse_factor(),
            Err(SyntaxError::InvalidQuantifierRange(2, 5))
        );
    }

    #[test]
    fn test_parse_quantifier_invalid_syntax() {
        let mut parser = Parser::new("a{5,)}");
        assert_eq!(
            parser.parse_factor(),
            Err(SyntaxError::InvalidNumberFormatInRangeDefinition)
        );
    }

    #[test]
    fn test_parse_quantifier_zero() {
        let mut parser = Parser::new("a{0,3}");
        assert_eq!(
            parser.parse_factor(),
            Ok(Factor {
                atom: Atom::Char('a'),
                min: 0,
                max: UpperBound::Exactly(3),
            })
        );
    }

    #[test]
    fn test_parse_complex_with_quantifiers() {
        let mut parser = Parser::new("ab{2,4}c");
        assert_eq!(
            parser.parse(),
            Ok(Regex {
                expression: Expression::Term(Term::Concatenation(
                    Factor {
                        atom: Atom::Char('a'),
                        min: 1,
                        max: UpperBound::Exactly(1),
                    },
                    Box::new(Term::Concatenation(
                        Factor {
                            atom: Atom::Char('b'),
                            min: 2,
                            max: UpperBound::Exactly(4),
                        },
                        Box::new(Term::Factor(Factor {
                            atom: Atom::Char('c'),
                            min: 1,
                            max: UpperBound::Exactly(1),
                        }))
                    ))
                )),
                start_anchor: false,
                end_anchor: false
            })
        );
    }
}
