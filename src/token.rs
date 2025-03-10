use std::fmt::Display;

use crate::ast::literal::Literal;

#[derive(Debug, PartialEq)]
pub enum Token {
    Literal(Literal),

    Ident(String),

    Ctrl(char),
    Op(String),

    Fn,

    If,
    Else,

    For,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Literal(a) => a.to_string(),
                Self::Ident(a) => a.clone(),
                Self::Ctrl(ch) => ch.to_string(),
                Self::Op(op) => op.clone(),

                Self::Fn => "fn".to_string(),

                Self::If => "if".to_string(),
                Self::Else => "else".to_string(),
                Self::For => "for".to_string(),
            }
        )
    }
}
