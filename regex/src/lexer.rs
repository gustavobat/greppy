use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'s> {
    pub origin: &'s str,
    pub offset: usize,
    pub kind: TokenKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Caret,
    Char(char),
    DollarSign,
    Dot,
    Escaped(char),
    LeftBrace,
    LeftBracket,
    LeftParen,
    Minus,
    Pipe,
    Plus,
    QuestionMark,
    RightBrace,
    RightBracket,
    RightParen,
    Star,
}

#[derive(Debug, Clone)]
pub(crate) struct Lexer<'s> {
    whole: &'s str,
    rest: &'s str,
    byte: usize,
    peeked: Option<Result<Token<'s>, TokenError>>,
}

#[derive(Error, Clone, Debug, PartialEq, Eq)]
pub enum TokenError {
    #[error("Unsupported token: '{0}'")]
    Unsupported(String),
    #[error("Invalid position of anchor '{0}'")]
    InvalidAnchorPosition(char),
    #[error("Qualifier '{0}' must be preceded by another character")]
    QualifierNotPreceded(char),
    #[error("Unclosed bracket")]
    Unclosed,
    #[error("Unexpected EOF")]
    UnexpectedEOF,
    #[error("Expected {0:?} got {1}")]
    UnexpectedToken(TokenKind, String),
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'s>, TokenError>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }
        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Result<Token<'s>, TokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.peeked.take() {
            return Some(next);
        }

        let mut chars = self.rest.chars();
        let c = chars.next()?;
        let c_at = self.byte;
        let c_str = &self.rest[..c.len_utf8()];
        self.rest = chars.as_str();
        self.byte += c.len_utf8();

        let make_token = move |kind: TokenKind| {
            Some(Ok(Token {
                kind,
                offset: c_at,
                origin: c_str,
            }))
        };

        let escaped = match c {
            '^' => return make_token(TokenKind::Caret),
            '$' => return make_token(TokenKind::DollarSign),
            '.' => return make_token(TokenKind::Dot),
            '{' => return make_token(TokenKind::LeftBrace),
            '[' => return make_token(TokenKind::LeftBracket),
            '(' => return make_token(TokenKind::LeftParen),
            '-' => return make_token(TokenKind::Minus),
            '|' => return make_token(TokenKind::Pipe),
            '+' => return make_token(TokenKind::Plus),
            '?' => return make_token(TokenKind::QuestionMark),
            '}' => return make_token(TokenKind::RightBrace),
            ']' => return make_token(TokenKind::RightBracket),
            ')' => return make_token(TokenKind::RightParen),
            '*' => return make_token(TokenKind::Star),
            '\\' => true,
            c => return make_token(TokenKind::Char(c)),
        };

        if escaped {
            let escaped = match self.rest.chars().next() {
                Some(escaped) => escaped,
                None => return Some(Err(TokenError::UnexpectedEOF)),
            };

            let c_at = self.byte - 1;
            self.byte += escaped.len_utf8();
            let c_str = &self.whole[c_at..self.byte];
            self.rest = &self.rest[escaped.len_utf8()..];
            Some(Ok(Token {
                kind: TokenKind::Escaped(escaped),
                offset: c_at,
                origin: c_str,
            }))
        } else {
            Some(Err(TokenError::Unsupported(c.to_string())))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_peek() {
        let mut lexer = Lexer::new("^🦝$");
        assert_eq!(
            lexer.peek(),
            Some(&Ok(Token {
                kind: TokenKind::Caret,
                offset: 0,
                origin: "^"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Caret,
                offset: 0,
                origin: "^"
            }))
        );
        assert_eq!(
            lexer.peek(),
            Some(&Ok(Token {
                kind: TokenKind::Char('🦝'),
                offset: 1,
                origin: "🦝"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Char('🦝'),
                offset: 1,
                origin: "🦝"
            }))
        );
        assert_eq!(
            lexer.peek(),
            Some(&Ok(Token {
                kind: TokenKind::DollarSign,
                offset: 5,
                origin: "$"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::DollarSign,
                offset: 5,
                origin: "$"
            }))
        );
        assert_eq!(lexer.peek(), None);
    }

    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new("^$.*🦝{[(-|+?}])");
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Caret,
                offset: 0,
                origin: "^"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::DollarSign,
                offset: 1,
                origin: "$"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Dot,
                offset: 2,
                origin: "."
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Star,
                offset: 3,
                origin: "*"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Char('🦝'),
                offset: 4,
                origin: "🦝"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::LeftBrace,
                offset: 8,
                origin: "{"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::LeftBracket,
                offset: 9,
                origin: "["
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::LeftParen,
                offset: 10,
                origin: "("
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Minus,
                offset: 11,
                origin: "-"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Pipe,
                offset: 12,
                origin: "|"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Plus,
                offset: 13,
                origin: "+"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::QuestionMark,
                offset: 14,
                origin: "?"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::RightBrace,
                offset: 15,
                origin: "}"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::RightBracket,
                offset: 16,
                origin: "]"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::RightParen,
                offset: 17,
                origin: ")"
            }))
        );
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_lexer_escaped() {
        let mut lexer = Lexer::new(r"\w\s\d");
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Escaped('w'),
                offset: 0,
                origin: r"\w"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Escaped('s'),
                offset: 2,
                origin: r"\s"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Escaped('d'),
                offset: 4,
                origin: r"\d"
            }))
        );
        assert_eq!(lexer.next(), None);
    }
}
