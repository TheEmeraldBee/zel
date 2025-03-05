use std::fmt::{Debug, Display};

#[derive(PartialEq, Clone)]
pub enum Literal {
    /// A comptime number, can be cast to any number type based on usage
    Num(i64),

    Bool(bool),
}

impl Literal {}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Num(n) => n.to_string(),
                Self::Bool(b) => b.to_string(),
            }
        )
    }
}
