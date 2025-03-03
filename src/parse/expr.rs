use super::ops::*;
use crate::Spanned;
use crate::types::Type;
use crate::value::Value;

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Error,
    Value(Value<'src>),
    List(Vec<Spanned<Self>>),

    Local(&'src str),

    Var(&'src str, Box<Spanned<Self>>),
    Const(&'src str, Box<Spanned<Self>>),
    Set(&'src str, SetOp, Box<Spanned<Self>>),

    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),

    Block(Box<Spanned<Self>>),

    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Monary(Box<Spanned<Self>>, MonadicOp),

    If(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),

    For(
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
        Box<Spanned<Self>>,
    ),
    While(Box<Spanned<Self>>, Box<Spanned<Self>>),

    Break,
    Continue,
    Return(Box<Spanned<Self>>),

    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),

    Func(Vec<(&'src str, Type)>, (Box<Spanned<Self>>, Type)),
}
