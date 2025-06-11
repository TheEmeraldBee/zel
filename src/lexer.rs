use crate::{ast::literal::Literal, token::Token};
use thiserror::Error;

#[derive(Error, Clone, Debug)]
pub enum LexError {
    #[error("invalid char `{0}` found")]
    Unknown(char),

    #[error("expected `{0}`, found EOF")]
    ExpectedFoundEof(&'static str),

    #[error("invalid unicode escape sequence")]
    InvalidUnicodeEscape,

    #[error("character literal cannot be empty")]
    EmptyCharLiteral,

    #[error("character literal contains too many characters")]
    TooManyCharsInLiteral,

    #[error("character literal value exceeds 8 bits")]
    CharLiteralExceeds8Bits,
}

pub struct Lexer {
    cur: isize,
    src: Vec<char>,
}

/// Returns whether the given char is a valid operator character
fn op(ch: char) -> bool {
    "*/+-=><!?&.".contains(ch)
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

    /// Parses an escape sequence and returns the resulting character.
    /// This function handles all Rust-supported escape sequences.
    fn parse_escape_sequence(&mut self) -> Result<char, LexError> {
        // Assume '\\' has already been consumed and self.cur is at the escaped char.
        let escaped_char = self.get();

        let result_char = match escaped_char {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '\\' => '\\',
            '"' => '"',
            '\'' => '\'',
            '0' => '\0',
            'x' => {
                // Hex escape: \xXX
                // Check if there are enough characters for two hex digits
                if (self.cur + 2) as usize >= self.src.len() {
                    return Err(LexError::ExpectedFoundEof("hex digits for \\x"));
                }
                let hex_str: String = self.src[(self.cur + 1) as usize..(self.cur + 3) as usize]
                    .iter()
                    .collect();
                self.cur += 2; // Consume the two hex digits
                u8::from_str_radix(&hex_str, 16)
                    .map(|v| v as char)
                    .map_err(|_| LexError::InvalidUnicodeEscape)?
            }
            'u' => {
                // Unicode escape: \u{XXXXXX}
                // Check for '{'
                if (self.cur + 1) as usize >= self.src.len()
                    || self.src[(self.cur + 1) as usize] != '{'
                {
                    return Err(LexError::InvalidUnicodeEscape);
                }
                self.advance(); // Consume '{'

                let mut unicode_hex_digits = String::new();
                loop {
                    // Check for EOF before advancing to the next hex digit or '}'
                    if (self.cur + 1) as usize >= self.src.len() {
                        return Err(LexError::ExpectedFoundEof("} for \\u{}"));
                    }
                    self.advance();
                    let uni_ch = self.get();
                    if uni_ch == '}' {
                        break;
                    }
                    if !uni_ch.is_ascii_hexdigit() {
                        return Err(LexError::InvalidUnicodeEscape);
                    }
                    unicode_hex_digits.push(uni_ch);
                }

                let value = u32::from_str_radix(&unicode_hex_digits, 16)
                    .map_err(|_| LexError::InvalidUnicodeEscape)?;
                char::from_u32(value).ok_or(LexError::InvalidUnicodeEscape)?
            }
            _ => {
                // Unrecognized escape sequence, return the literal backslash and the character
                return Err(LexError::Unknown(escaped_char));
            }
        };
        Ok(result_char)
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

            '"' => {
                let mut string_content_chars = Vec::new();
                let mut escaped = false;

                loop {
                    // Check for EOF before advancing
                    if (self.cur + 1) as usize >= self.src.len() {
                        return Err(LexError::ExpectedFoundEof("\""));
                    }

                    // Advance to the next character
                    self.advance();
                    let ch = self.get();

                    if escaped {
                        // Handle escape sequence using the new helper function
                        let parsed_char = self.parse_escape_sequence()?;
                        string_content_chars.push(parsed_char);
                        escaped = false;
                    } else if ch == '\\' {
                        escaped = true;
                    } else if ch == '"' {
                        break; // End of string literal
                    } else {
                        string_content_chars.push(ch);
                    }
                }

                let string_value: String = string_content_chars.into_iter().collect();
                Ok(Some(Token::Literal(Literal::String(string_value))))
            }

            // Handle character literals
            '\'' => {
                // Check for EOF immediately after opening quote
                if (self.cur + 1) as usize >= self.src.len() {
                    return Err(LexError::ExpectedFoundEof("'"));
                }
                self.advance(); // Consume the opening '\''

                let ch = self.get(); // Get the character inside the literal

                if ch == '\'' {
                    return Err(LexError::EmptyCharLiteral);
                }

                let char_literal = if ch == '\\' {
                    // It's an escape sequence
                    // Ensure there's a character to escape
                    if (self.cur + 1) as usize >= self.src.len() {
                        return Err(LexError::ExpectedFoundEof("escaped character"));
                    }
                    self.advance(); // Consume '\\'
                    Some(self.parse_escape_sequence()?)
                } else {
                    // It's a regular character
                    Some(ch)
                };

                if (self.cur + 1) as usize >= self.src.len() {
                    return Err(LexError::ExpectedFoundEof("'"));
                }
                self.advance(); // Move to the char after the literal's content

                if self.get() != '\'' {
                    return Err(LexError::TooManyCharsInLiteral);
                }

                let char_val = char_literal.ok_or(LexError::EmptyCharLiteral)?;

                // Ensure the character can fit into 8 bits (i.e., is a single-byte UTF-8 character)
                if char_val.len_utf8() != 1 {
                    return Err(LexError::CharLiteralExceeds8Bits);
                }

                Ok(Some(Token::Literal(Literal::Num(char_val as i64))))
            }

            ch if ctrl(ch) => Ok(Some(Token::Ctrl(ch))),

            ch if op(ch) => {
                let (_found, rest) = self.collect_until(|lex| !op(lex.get()));
                Ok(Some(Token::Op(rest.into_iter().collect())))
            }

            ch if ident(ch) => {
                let (_found, rest) = self.collect_until(|lex| !ident(lex.get()));
                let ident = rest.into_iter().collect::<String>();

                Ok(Some(match ident.as_str() {
                    "true" => Token::Literal(Literal::Bool(true)),
                    "false" => Token::Literal(Literal::Bool(false)),

                    "if" => Token::If,
                    "else" => Token::Else,

                    "for" => Token::For,

                    "let" => Token::Let,
                    "mut" => Token::Mut,

                    "struct" => Token::Struct,

                    "fn" => Token::Fn,

                    "const" => Token::Const,
                    "extern" => Token::Extern,

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
                self.cur -= 1;
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
        let tokens = Lexer::lex("/+-*.").unwrap();
        assert_eq!(vec![Token::Op("/+-*.".to_string())], tokens)
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

    #[test]
    fn test_string_basic_escapes() {
        let tokens = Lexer::lex(r#""Hello\nWorld\t\"Inside\\Here\'\0""#).unwrap();
        assert_eq!(
            vec![Token::Literal(Literal::String(
                "Hello\nWorld\t\"Inside\\Here'\0".to_string()
            )),],
            tokens
        );
    }

    #[test]
    fn test_string_hex_escapes() {
        let tokens = Lexer::lex(r#""\x41\x62\x7C""#).unwrap(); // A, b, |
        assert_eq!(
            vec![Token::Literal(Literal::String("Ab|".to_string()))],
            tokens
        );
    }

    #[test]
    fn test_string_unicode_escapes() {
        let tokens = Lexer::lex(r#""\u{0041}\u{1F600}""#).unwrap(); // A, grinning face
        assert_eq!(
            vec![Token::Literal(Literal::String("A😀".to_string()))],
            tokens
        );
    }

    #[test]
    fn test_string_mixed_escapes() {
        let tokens = Lexer::lex(r#""Line1\n\x20Line2\u{2764}""#).unwrap(); // Line1, newline, space, Line2, heart
        assert_eq!(
            vec![Token::Literal(Literal::String(
                "Line1\n Line2❤".to_string()
            ))],
            tokens
        );
    }

    #[test]
    fn test_string_invalid_hex_escape() {
        let err = Lexer::lex(r#""\xGZ""#).unwrap_err();
        assert!(matches!(err, LexError::InvalidUnicodeEscape));
    }

    #[test]
    fn test_string_incomplete_hex_escape() {
        let err = Lexer::lex(r#""\x1""#).unwrap_err();
        assert!(matches!(err, LexError::ExpectedFoundEof(_)));
    }

    #[test]
    fn test_string_incomplete_unicode_escape_start() {
        let err = Lexer::lex(r#""\u""#).unwrap_err();
        assert!(matches!(err, LexError::InvalidUnicodeEscape));
    }

    #[test]
    fn test_string_incomplete_unicode_escape_brace() {
        let err = Lexer::lex(r#""\u{123""#).unwrap_err();
        assert!(matches!(err, LexError::ExpectedFoundEof(_)));
    }

    #[test]
    fn test_string_invalid_unicode_hex() {
        let err = Lexer::lex(r#""\u{G}""#).unwrap_err();
        assert!(matches!(err, LexError::InvalidUnicodeEscape));
    }

    #[test]
    fn test_string_invalid_unicode_scalar() {
        // U+110000 is beyond the valid Unicode range
        let err = Lexer::lex(r#""\u{110000}""#).unwrap_err();
        assert!(matches!(err, LexError::InvalidUnicodeEscape));
    }

    // New Tests for Character Literals
    #[test]
    fn test_char_literal_basic() {
        let tokens = Lexer::lex(r#"'a'"#).unwrap();
        assert_eq!(vec![Token::Literal(Literal::Num('a' as i64))], tokens);
    }

    #[test]
    fn test_char_literal_escaped() {
        let tokens = Lexer::lex(r#"'\n'"#).unwrap();
        assert_eq!(vec![Token::Literal(Literal::Num('\n' as i64))], tokens);

        let tokens = Lexer::lex(r#"'\t'"#).unwrap();
        assert_eq!(vec![Token::Literal(Literal::Num('\t' as i64))], tokens);

        let tokens = Lexer::lex(r#"'\r'"#).unwrap();
        assert_eq!(vec![Token::Literal(Literal::Num('\r' as i64))], tokens);

        let tokens = Lexer::lex(r#"'\\'"#).unwrap();
        assert_eq!(vec![Token::Literal(Literal::Num('\\' as i64))], tokens);

        let tokens = Lexer::lex(r#"'\"'"#).unwrap();
        assert_eq!(vec![Token::Literal(Literal::Num('"' as i64))], tokens);

        let tokens = Lexer::lex(r#"'\''"#).unwrap();
        assert_eq!(vec![Token::Literal(Literal::Num('\'' as i64))], tokens);

        let tokens = Lexer::lex(r#"'\0'"#).unwrap();
        assert_eq!(vec![Token::Literal(Literal::Num('\0' as i64))], tokens);
    }

    #[test]
    fn test_char_literal_hex_escape() {
        let tokens = Lexer::lex(r#"'\x41'"#).unwrap(); // 'A'
        assert_eq!(vec![Token::Literal(Literal::Num(0x41 as i64))], tokens);
    }

    #[test]
    fn test_char_literal_unicode_ascii() {
        let tokens = Lexer::lex(r#"'\u{0041}'"#).unwrap(); // 'A'
        assert_eq!(vec![Token::Literal(Literal::Num(0x41 as i64))], tokens);
    }

    #[test]
    fn test_char_literal_empty() {
        let err = Lexer::lex(r#"''"#).unwrap_err();
        assert!(matches!(err, LexError::EmptyCharLiteral));
    }

    #[test]
    fn test_char_literal_too_many_chars() {
        let err = Lexer::lex(r#"'ab'"#).unwrap_err();
        assert!(matches!(err, LexError::TooManyCharsInLiteral));
    }

    #[test]
    fn test_char_literal_too_many_chars_escaped() {
        let err = Lexer::lex(r#"'\nZ'"#).unwrap_err();
        assert!(matches!(err, LexError::TooManyCharsInLiteral));
    }

    #[test]
    fn test_char_literal_invalid_hex() {
        let err = Lexer::lex(r#"'\xGZ'"#).unwrap_err();
        assert!(matches!(err, LexError::InvalidUnicodeEscape));
    }

    #[test]
    fn test_char_literal_incomplete_hex() {
        let err = Lexer::lex(r#"'\x1'"#).unwrap_err();
        assert!(matches!(err, LexError::ExpectedFoundEof(_)));
    }

    #[test]
    fn test_char_literal_incomplete_unicode_start() {
        let err = Lexer::lex(r#"'\u'"#).unwrap_err();
        assert!(matches!(err, LexError::InvalidUnicodeEscape));
    }

    #[test]
    fn test_char_literal_incomplete_unicode_brace() {
        let err = Lexer::lex(r#"'\u{123'"#).unwrap_err();
        assert!(matches!(err, LexError::ExpectedFoundEof(_)));
    }

    #[test]
    fn test_char_literal_unicode_exceeds_8bit() {
        let err = Lexer::lex(r#"'\u{0100}'"#).unwrap_err();
        assert!(matches!(err, LexError::CharLiteralExceeds8Bits));

        let err = Lexer::lex(r#"'\u{1F600}'"#).unwrap_err();
        assert!(matches!(err, LexError::CharLiteralExceeds8Bits));
    }

    #[test]
    fn test_char_literal_non_ascii_single_byte() {
        let err = Lexer::lex(r#"'©'"#).unwrap_err();
        assert!(matches!(err, LexError::CharLiteralExceeds8Bits));
    }
}
