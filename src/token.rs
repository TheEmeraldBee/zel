use std::fmt::Display;

use crate::ast::literal::Literal;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Literal(Literal),

    Ident(String),

    Ctrl(char),
    Op(String),

    Let,
    Mut,

    Fn,

    If,
    Else,

    For,

    Struct,

    Const,
    Extern,

    This,
    Return,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Literal(val) => format!("literal: `{}`", val),
                Self::Ident(ident) => format!("ident: `{}`", ident),
                Self::Ctrl(ch) => format!("ctrl: `{}`", ch),
                Self::Op(op) => format!("op: `{}`", op),

                Self::Let => "let".to_string(),
                Self::Mut => "mut".to_string(),

                Self::Fn => "fn".to_string(),

                Self::If => "if".to_string(),
                Self::Else => "else".to_string(),
                Self::For => "for".to_string(),

                Self::Struct => "struct".to_string(),

                Self::Const => "const".to_string(),
                Self::Extern => "extern".to_string(),

                Self::This => "this".to_string(),
                Self::Return => "return".to_string(),
            }
        )
    }
}
