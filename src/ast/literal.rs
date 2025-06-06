use std::fmt::{Debug, Display};

#[derive(PartialEq, Clone, Hash)]
pub enum Literal {
    /// A comptime number, can be cast to any number type based on usage
    Num(i64),

    /// A condition, can be true or false
    Bool(bool),

    /// Represents a string slice, aka a static length string
    String(String),
}

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
                Self::String(s) => format!("{s}"),
            }
        )
    }
}
