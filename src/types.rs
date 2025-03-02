use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(Primitive),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Primitive(p) => p.to_string(),
            }
        )
    }
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

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Primitive::Null => "null".to_string(),
                Primitive::Num => "num".to_string(),
                Primitive::Bool => "bool".to_string(),
                Primitive::String => "string".to_string(),
                Primitive::List(t) => format!("[{t}]"),
                Primitive::Func(args, ret) => format!(
                    "fn ({}) -> {}",
                    args.iter()
                        .map(|x| x.to_string())
                        .reduce(|prev, this| format!("{prev}, {this}"))
                        .unwrap_or("".to_string()),
                    ret
                ),
                Primitive::RustFunc(_) => todo!(),
            }
        )
    }
}

/*
const Primitive = enum {
    null,

    num,

    list(Type),

    func([Type], Type),
};

.null;
.Null;

.list(.num);
.List(.Num);
*/
