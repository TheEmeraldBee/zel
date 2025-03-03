use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum MonadicOp {
    Neg,
    Not,
}

impl Display for MonadicOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Neg => "-",
                Self::Not => "!",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum SetOp {
    Set,
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for SetOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Set => "=",
                Self::Add => "+=",
                Self::Sub => "-=",
                Self::Mul => "*=",
                Self::Div => "/=",
            }
        )
    }
}

#[derive(Clone, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,

    Or,
    And,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",

                Self::Eq => "==",
                Self::NotEq => "!=",

                Self::Less => "<",
                Self::LessEq => "<=",

                Self::Greater => ">",
                Self::GreaterEq => ">=",

                Self::Or => "||",
                Self::And => "&&",
            }
        )
    }
}
