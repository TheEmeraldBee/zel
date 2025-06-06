use std::fmt::{Debug, Display};

#[derive(Copy, Clone, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
}

impl BinaryOp {
    pub fn parsed(string: impl AsRef<str>) -> Option<BinaryOp> {
        let string = string.as_ref();
        Some(match string {
            "+" => BinaryOp::Add,
            "-" => BinaryOp::Sub,
            "*" => BinaryOp::Mul,
            "/" => BinaryOp::Div,
            "==" => BinaryOp::Eq,
            "!=" => BinaryOp::Ne,
            "<" => BinaryOp::Lt,
            "<=" => BinaryOp::Lte,
            ">" => BinaryOp::Gt,
            ">=" => BinaryOp::Gte,
            _ => return None,
        })
    }

    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Lte
            | BinaryOp::Gt
            | BinaryOp::Gte => 0,

            BinaryOp::Add | BinaryOp::Sub => 1,
            BinaryOp::Mul | BinaryOp::Div => 2,
        }
    }
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
                BinaryOp::Ne => "!=",
                BinaryOp::Lt => "<",
                BinaryOp::Lte => "<=",
                BinaryOp::Gt => ">",
                BinaryOp::Gte => ">=",
            }
        )
    }
}

impl Debug for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
