#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
}

impl Type {
    pub fn can_add(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Num) => true,
            Self::Primitive(Primitive::String) => true,
            Self::Primitive(Primitive::List(_)) => true,
            _ => false,
        }
    }
    pub fn can_sub(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Num) => true,
            _ => false,
        }
    }
    pub fn can_mul(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Num) => true,
            _ => false,
        }
    }
    pub fn can_div(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Num) => true,
            _ => false,
        }
    }
    pub fn can_neg(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Num) => true,
            _ => false,
        }
    }
    pub fn can_not(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Bool) => true,
            _ => false,
        }
    }
    pub fn can_eq(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Bool) => true,
            Self::Primitive(Primitive::Num) => true,
            Self::Primitive(Primitive::String) => true,
            Self::Primitive(Primitive::List(list_type)) => list_type.can_eq(),
            _ => false,
        }
    }
    pub fn can_comp(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Bool) => true,
            Self::Primitive(Primitive::Num) => true,
            Self::Primitive(Primitive::String) => true,
            _ => false,
        }
    }
    pub fn can_bool(&self) -> bool {
        match self {
            Self::Primitive(Primitive::Bool) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Primitive {
    Null,

    Num,
    Bool,
    String,
    List(Box<Type>),

    Func(Vec<Type>, Box<Type>),
    RustFunc(Box<Type>),
}
