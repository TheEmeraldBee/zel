use std::fmt::{Debug, Display};

#[derive(PartialEq, Clone)]
pub enum Literal {
    /// A comptime number, can be cast to any number type based on usage
    Num(i64),

    Bool(bool),
}

pub fn min_bits(n: i64) -> u16 {
    return 64;
    if n == 0 {
        return 8;
    }

    if n > 0 {
        let val = n as u64;
        let bits_needed = 64 - val.leading_zeros();
        if bits_needed <= 8 {
            return 8;
        } else if bits_needed <= 16 {
            return 16;
        } else if bits_needed <= 32 {
            return 32;
        } else {
            return 64;
        }
    } else {
        // n < 0

        if n == i64::MIN {
            return 64;
        }

        if n >= -128 {
            return 8;
        } else if n >= -32768 {
            return 16;
        } else if n >= -2147483648 {
            return 32;
        } else {
            return 64;
        }
    }
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
            }
        )
    }
}
