use thiserror::Error;

use crate::{ast::literal::Literal, token::Token};
use std::{iter::*, str::Chars};

macro_rules! otry {
    ($fallible:expr) => {
        match $fallible {
            Ok(t) => t,
            Err(e) => return Some(Err(e.into())),
        }
    };
}

#[derive(Error, Debug)]
pub enum LexError {
    #[error("invalid char `{0}` found")]
    Unknown(char),
}

/// Utility Struct for simpler lexer control flow
pub struct Lex<'src> {
    cur: char,
    iter: Peekable<Chars<'src>>,
}

impl<'src> Lex<'src> {
    /// Build a new TokenIter from the given input
    pub fn new(input: &'src str) -> Self {
        Self {
            cur: '\0',
            iter: input.chars().peekable(),
        }
    }

    /// advances the text, returning the character
    fn advance(&mut self) -> Option<()> {
        self.cur = self.iter.next()?;

        Some(())
    }

    /// Simple wrapper to check if character is what is wanted
    fn is(&mut self, ch: &char) -> bool {
        &self.cur == ch
    }

    fn then(&mut self, ch: &char) -> bool {
        self.iter.peek() == Some(ch)
    }

    /// Returns an iterator that will take characters while they are following the given condition
    fn take_while(&mut self, cond: &'static dyn Fn(&char) -> bool) -> impl Iterator<Item = char> {
        once(self.cur).chain(from_fn(|| self.iter.by_ref().next_if(|ch| cond(ch))))
    }

    fn ignore_until(&mut self, cond: impl Fn(&mut Self) -> bool) {
        while self.advance().is_some() {
            if cond(self) {
                return;
            }
        }
    }
}

impl Iterator for Lex<'_> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        // Move to next character
        self.advance()?;

        Some(Ok(match self.cur {
            // Recursively call next if finding whitespace
            ch if ch.is_whitespace() => otry!(self.next()?),

            '/' => {
                self.advance()?;

                if self.is(&'/') {
                    // Ignore input until newline
                    self.take_while(&|ch| ch != &'\n').for_each(drop);

                    // Return the result of the next item
                    otry!(self.next()?)
                } else if self.is(&'*') {
                    // Ignore output until following condition met
                    self.ignore_until(|lex: &mut Lex<'_>| lex.is(&'*') && lex.then(&'/'));

                    // Also ignore the '/'
                    self.advance()?;
                    otry!(self.next()?)
                } else {
                    Token::Op("/".to_string() + &self.take_while(&op).collect::<String>())
                }
            }

            // Match any control characters
            ch if ctrl(&ch) => Token::Ctrl(ch),

            // Match any operators
            ch if op(&ch) => Token::Op(self.take_while(&op).collect()),

            // Match a number and parse it into a literal
            '0'..='9' => Token::Literal(Literal::Num(
                self.take_while(&|ch| ch.is_ascii_digit())
                    .collect::<String>()
                    .parse()
                    .expect("Collected string should be number"),
            )),

            // Check if it could be an identifier
            ch if ident(&ch) => {
                // Collect the string identifier
                let ident = self.take_while(&ident).collect::<String>();

                // Check for keywords or make Ident Token
                match ident.as_str() {
                    "true" => Token::Literal(Literal::Bool(true)),
                    "false" => Token::Literal(Literal::Bool(false)),

                    "fn" => Token::Fn,

                    "if" => Token::If,
                    "else" => Token::Else,

                    "for" => Token::For,
                    _ => Token::Ident(ident),
                }
            }

            // Invalid character check, should be error eventually
            ch => otry!(Err(LexError::Unknown(ch))),
        }))
    }
}

/// Returns whether the given char is a valid operator character
fn op(ch: &char) -> bool {
    "*/+-=><!?".contains(*ch)
}

/// Returns whether the given char is a control character
fn ctrl(ch: &char) -> bool {
    "{}()[];,".contains(*ch)
}

/// Returns whether the given char is a valid identifier char
fn ident(ch: &char) -> bool {
    (ch.is_ascii_alphanumeric() || ch == &'_') && !ch.is_whitespace()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_comment() {
        let tokens = Lex::new("// Hi Mom\nhi")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(vec![Token::Ident("hi".to_string())], tokens)
    }

    #[test]
    fn test_block_comment() {
        let tokens = Lex::new("/* Hi Mom*/hi")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(vec![Token::Ident("hi".to_string())], tokens)
    }

    #[test]
    fn test_ident() {
        let tokens = Lex::new("_hello_world hi_mom")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            vec![
                Token::Ident("_hello_world".to_string()),
                Token::Ident("hi_mom".to_string()),
            ],
            tokens
        );
    }

    #[test]
    fn test_keywords() {
        let tokens = Lex::new("if else for true false")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            vec![
                Token::If,
                Token::Else,
                Token::For,
                Token::Literal(Literal::Bool(true)),
                Token::Literal(Literal::Bool(false)),
            ],
            tokens
        )
    }

    #[test]
    fn test_ops() {
        let tokens = Lex::new("/+-*").map(|x| x.unwrap()).collect::<Vec<_>>();
        assert_eq!(vec![Token::Op("/+-*".to_string())], tokens)
    }

    #[test]
    fn test_nums() {
        let tokens = Lex::new("1234567890 12 18 0")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            vec![
                Token::Literal(Literal::Num(1234567890)),
                Token::Literal(Literal::Num(12)),
                Token::Literal(Literal::Num(18)),
                Token::Literal(Literal::Num(0)),
            ],
            tokens
        )
    }

    #[test]
    fn test_if_else() {
        let tokens = Lex::new("if x == 5 {} else {}")
            .map(|x| x.unwrap())
            .collect::<Vec<_>>();

        assert_eq!(
            vec![
                Token::If,
                Token::Ident("x".to_string()),
                Token::Op("==".to_string()),
                Token::Literal(Literal::Num(5)),
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                Token::Else,
                Token::Ctrl('{'),
                Token::Ctrl('}'),
            ],
            tokens
        )
    }
}
