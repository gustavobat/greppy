use crate::regex::error::TokenError;

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

pub(crate) struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
    peeked: Option<Result<Token<'de>, TokenError>>,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'de>, TokenError>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }
        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, TokenError>;

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
    fn test_lexer() {
        let mut lexer = Lexer::new("^$.*{[(-|+?}])");
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
                kind: TokenKind::LeftBrace,
                offset: 4,
                origin: "{"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::LeftBracket,
                offset: 5,
                origin: "["
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::LeftParen,
                offset: 6,
                origin: "("
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Minus,
                offset: 7,
                origin: "-"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Pipe,
                offset: 8,
                origin: "|"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::Plus,
                offset: 9,
                origin: "+"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::QuestionMark,
                offset: 10,
                origin: "?"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::RightBrace,
                offset: 11,
                origin: "}"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::RightBracket,
                offset: 12,
                origin: "]"
            }))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token {
                kind: TokenKind::RightParen,
                offset: 13,
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
