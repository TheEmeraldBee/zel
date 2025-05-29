use std::fmt::Display;

use cranelift::prelude::Type;

#[derive(Copy, Clone, Debug)]
pub enum IntegerType {
    MinBits(u16),
    Bits(u16),
}

impl IntegerType {
    pub fn bits(&self) -> u16 {
        match self {
            Self::MinBits(a) => *a,
            Self::Bits(a) => *a,
        }
    }
}

#[derive(Clone, Debug)]
pub enum CompType {
    /// Represents an integer with x bits
    /// Booleans can be represented by Integer(1)
    /// A None bits value means that the value can be represented at compiletime
    Integer(IntegerType),

    /// Represents a function with x args and a specific return type
    Function(Vec<Self>, Box<Self>),

    /// In `zel` Null is a type, as values can not be null, and must be some value, or explicitly be possible to be none
    Null,

    /// Should never be checked, is a state meaning the type-system
    /// Doesn't know the expected type of a call
    /// This is used when the compiler isn't sure of a type,
    /// It will then solve this most of the time, and throw errors when it is unable to.
    Unknown,
}

impl CompType {
    /// Returns what would be the compiletime type as a cranelift IR type
    pub fn as_type(&self) -> Option<Type> {
        match self {
            Self::Integer(bits) => {
                println!("{}", bits.bits());
                Some(Type::int(bits.bits()).expect("Bit count should be valid"))
            }

            // This is a comptime type, so it can't be represented in cranelift.
            Self::Function(..) => None,

            // the Null type can not be represented as a value in cranelift.
            Self::Null => None,

            Self::Unknown => unreachable!("Unknown Shouldn't Be Read"),
        }
    }

    pub fn equal(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(a), Self::Integer(b)) => match (a, b) {
                (IntegerType::Bits(a), IntegerType::MinBits(b)) => a >= b,
                (IntegerType::MinBits(a), IntegerType::Bits(b)) => b >= a,
                (IntegerType::Bits(a), IntegerType::Bits(b)) => a == b,

                // These could both be i64s if needed, so it fits for sure
                (IntegerType::MinBits(_), IntegerType::MinBits(_)) => true,
            },
            (Self::Function(args, ret), Self::Function(args_b, ret_b)) => {
                args.iter().zip(args_b.iter()).all(|x| x.0.equal(x.1)) && ret.equal(ret_b)
            }
            (Self::Null, Self::Null) => true,
            (_, _) => false,
        }
    }
}

impl PartialEq for CompType {
    fn eq(&self, other: &Self) -> bool {
        self.equal(other)
    }
}

impl Display for CompType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Integer(i) => format!("int{}", i.bits()),
                Self::Function(a, r) => format!(
                    "fn{} -> {r}",
                    a.iter()
                        .map(|x| x.to_string())
                        .reduce(|l, r| format!("{l}, {r}"))
                        .unwrap_or_default()
                ),
                Self::Null => "null".to_string(),
                Self::Unknown => "unknown".to_string(),
            }
        )
    }
}
