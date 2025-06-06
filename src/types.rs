use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub fields: Vec<(String, Type)>,
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "struct: {{{}}}",
            self.fields
                .iter()
                .map(|x| format!("{}: {}", x.0, x.1))
                .reduce(|l, r| format!("{l}, {r}"))
                .unwrap_or_default()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// An integer with the given number of bits.
    Integer(u8),
    Bool,
    String,

    Type,

    Array(usize, Box<Self>),
    Slice(Box<Self>),

    Struct(Struct),

    Func,

    Null,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Integer(bits) => format!("i{bits}"),
                Self::Bool => "bool".to_string(),
                Self::String => "string".to_string(),

                Self::Type => "type".to_string(),

                Self::Struct(s) => s.to_string(),

                Self::Func => "func".to_string(),

                Self::Array(size, element_type) => format!("[{element_type}; {size}]"),
                Self::Slice(element_type) => format!("[{element_type}]"),

                Self::Null => "null".to_string(),
            }
        )
    }
}
