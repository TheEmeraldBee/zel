#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Bool(bool),
    Num(f64),
    Str(&'src str),
    Op(&'src str),
    Ctrl(char),
    Ident(&'src str),

    Fn,
    Var,
    Const,

    For,

    Continue,
    Break,
    Return,

    If,
    Else,

    This,
}
