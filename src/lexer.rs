use thiserror::Error;

use crate::{ast::literal::Literal, token::Token};

#[derive(Error, Clone, Debug)]
pub enum LexError {
    #[error("invalid char `{0}` found")]
    Unknown(char),

    #[error("expected `{0}`, found EOF")]
    ExpectedFoundEof(&'static str),
}

pub struct Lexer {
    cur: isize,
    src: Vec<char>,
}

/// Returns whether the given char is a valid operator character
fn op(ch: char) -> bool {
    "*/+-=><!?".contains(ch)
}

/// Returns whether the given char is a control character
fn ctrl(ch: char) -> bool {
    "{}()[]:;,".contains(ch)
}

/// Returns whether the given char is a valid identifier char
fn ident(ch: char) -> bool {
    (ch.is_ascii_alphanumeric() || ch == '_') && !ch.is_whitespace()
}

impl Lexer {
    pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
        let mut lexer = Self {
            cur: -1,
            src: input.chars().collect(),
        };

        let mut tokens = vec![];

        while let Some(token) = lexer.next()? {
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn expect(res: bool, err_func: impl FnOnce() -> LexError) -> Result<(), LexError> {
        match res {
            true => Ok(()),
            false => Err(err_func()),
        }
    }

    fn next(&mut self) -> Result<Option<Token>, LexError> {
        if !self.advance() {
            return Ok(None);
        }
        match self.get() {
            ch if ch.is_whitespace() => self.next(),

            ch if ch.is_ascii_digit() => {
                let val = Ok(Some(Token::Literal(Literal::Num(
                    self.collect_until(|lex| !lex.get().is_ascii_digit())
                        .1
                        .into_iter()
                        .collect::<String>()
                        .parse()
                        .unwrap(),
                ))));

                self.cur -= 1;

                val
            }

            '/' => {
                if self.then("/") {
                    Lexer::expect(self.ignore_until("\n"), || {
                        LexError::ExpectedFoundEof("\\n")
                    })?;
                    self.next()
                } else if self.then("*") {
                    Lexer::expect(self.ignore_until("*/"), || LexError::ExpectedFoundEof("*/"))?;
                    self.next()
                } else {
                    let (_found, rest) = self.collect_until(|lex| !op(lex.get()));
                    Ok(Some(Token::Op(rest.into_iter().collect())))
                }
            }

            ch if ctrl(ch) => Ok(Some(Token::Ctrl(ch))),

            ch if op(ch) => {
                let (_found, rest) = self.collect_until(|lex| !op(lex.get()));
                Ok(Some(Token::Op(rest.into_iter().collect())))
            }

            ch if ident(ch) => {
                let (_found, rest) = self.collect_until(|lex| !ident(lex.get()));
                let ident = rest.into_iter().collect::<String>();

                self.cur -= 1;

                Ok(Some(match ident.as_str() {
                    "true" => Token::Literal(Literal::Bool(true)),
                    "false" => Token::Literal(Literal::Bool(false)),

                    "if" => Token::If,
                    "else" => Token::Else,

                    "for" => Token::For,

                    "let" => Token::Let,
                    "mut" => Token::Mut,

                    "fn" => Token::Fn,
                    "this" => Token::This,
                    "return" => Token::Return,

                    _ => Token::Ident(ident),
                }))
            }

            ch => Err(LexError::Unknown(ch)),
        }
    }

    fn advance(&mut self) -> bool {
        self.cur += 1;
        self.cur < self.src.len() as isize
    }

    fn get(&mut self) -> char {
        self.src[self.cur as usize]
    }

    fn is(&mut self, ch: char) -> bool {
        self.src[self.cur as usize] == ch
    }

    fn then(&mut self, stream: &str) -> bool {
        let start = self.cur;
        for ch in stream.chars() {
            if !self.advance() {
                self.cur = start;
                return false;
            }

            if !self.is(ch) {
                self.cur = start;
                return false;
            }
        }

        true
    }

    fn ignore_until(&mut self, stream: &str) -> bool {
        while !self.then(stream) {
            if !self.advance() {
                return false;
            }
        }
        true
    }

    fn collect_until(&mut self, func: impl Fn(&mut Self) -> bool) -> (bool, Vec<char>) {
        let mut chrs = vec![self.get()];

        loop {
            if !self.advance() {
                return (false, chrs);
            }

            if func(self) {
                return (true, chrs);
            }

            chrs.push(self.get())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_comment() {
        let tokens = Lexer::lex("// Hi Mom\nhi").unwrap();

        assert_eq!(vec![Token::Ident("hi".to_string())], tokens)
    }

    #[test]
    fn test_block_comment() {
        let tokens = Lexer::lex("/* Hi Mom*/hi").unwrap();
        assert_eq!(vec![Token::Ident("hi".to_string())], tokens)
    }

    #[test]
    fn test_ident() {
        let tokens = Lexer::lex("_hello_world hi_mom").unwrap();

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
        let tokens = Lexer::lex("if else for true false").unwrap();

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
        let tokens = Lexer::lex("/+-*").unwrap();
        assert_eq!(vec![Token::Op("/+-*".to_string())], tokens)
    }

    #[test]
    fn test_nums() {
        let tokens = Lexer::lex("1234567890 12 18 0").unwrap();

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
        let tokens = Lexer::lex("if x == 5 {} else {}").unwrap();

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
