use crate::{ast::literal::Literal, token::Token};
use std::{iter::*, str::Chars};

struct TokenIter<'src> {
    cur: char,
    iter: Peekable<Chars<'src>>,
}

impl<'src> TokenIter<'src> {
    fn new(input: &'src str) -> Self {
        Self {
            cur: '\0',
            iter: input.chars().peekable(),
        }
    }

    fn advance(&mut self) -> Option<&char> {
        self.cur = self.iter.next()?;

        Some(&self.cur)
    }

    fn take_while(&mut self, cond: &'static dyn Fn(&char) -> bool) -> impl Iterator<Item = char> {
        once(self.cur).chain(from_fn(|| self.iter.by_ref().next_if(|ch| cond(ch))))
    }
}

fn op(ch: &char) -> bool {
    "*/+-=><!?".contains(*ch)
}

fn ctrl(ch: &char) -> bool {
    "{}()[];,".contains(*ch)
}

fn ident(ch: &char) -> bool {
    (ch.is_ascii_alphanumeric() || ch == &'_') && !ch.is_whitespace()
}

pub fn lex(input: &str) -> Vec<Token> {
    let mut token_iter = TokenIter::new(input);
    let mut tokens = vec![];

    while let Some(ch) = token_iter.advance() {
        tokens.push(match ch {
            // Ignore whitespace characters
            ch if ch.is_whitespace() => continue,

            // Number Literal
            '0'..='9' => Token::Literal(Literal::Num(
                token_iter
                    .take_while(&|ch| ch.is_ascii_digit())
                    .collect::<String>()
                    .parse()
                    .expect("Collected string should be number"),
            )),

            // Control characters, like brackets, semicolons, commas, etc.
            ch if ctrl(ch) => Token::Ctrl(*ch),

            ch if op(ch) => Token::Op(token_iter.take_while(&op).collect::<String>()),

            // Is either a keyword or identifier, so collect the remaining chars and check for keyword!
            ch if ident(ch) => {
                // Parse the text
                let text = token_iter.take_while(&ident).collect::<String>();
                match text.as_str() {
                    "if" => Token::If,
                    "else" => Token::Else,

                    "for" => Token::For,

                    "fn" => Token::Fn,

                    _ => Token::Ident(text),
                }
            }

            _ => panic!("Invalid char `{}`", ch),
        });
    }

    tokens.push(Token::EOF);

    tokens
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ident() {
        let tokens = lex("_hello_world; hi_mom");

        assert_eq!(
            vec![
                Token::Ident("_hello_world".to_string()),
                Token::Ctrl(';'),
                Token::Ident("hi_mom".to_string()),
                Token::EOF
            ],
            tokens
        );
    }

    #[test]
    fn test_if_else() {
        let tokens = lex("if x == 5 {} else {}");

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
                Token::EOF,
            ],
            tokens
        )
    }
}
