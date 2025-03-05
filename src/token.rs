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

    EOF,
}
