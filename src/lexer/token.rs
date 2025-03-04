use std::fmt::Display;

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

impl<'src> Display for Token<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bool(b) => b.to_string(),
                Self::Num(n) => n.to_string(),
                Self::Str(s) => s.to_string(),
                Self::Op(op) => op.to_string(),
                Self::Ctrl(chr) => chr.to_string(),
                Self::Ident(i) => i.to_string(),

                Self::Fn => "fn".to_string(),
                Self::Var => "var".to_string(),
                Self::Const => "const".to_string(),

                Self::For => "for".to_string(),

                Self::Continue => "continue".to_string(),
                Self::Break => "break".to_string(),
                Self::Return => "return".to_string(),

                Self::If => "if".to_string(),
                Self::Else => "else".to_string(),

                Self::This => "this".to_string(),
            }
        )
    }
}
