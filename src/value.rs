use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    Span, Spanned,
    error::Error,
    parse::Expr,
    types::{Primitive, Type},
};

#[derive(Clone)]
pub enum Value<'src> {
    Null,
    Bool(bool),
    Num(f64),
    Str(String),
    List(Vec<Self>),
    Func(Vec<&'src str>, Box<Spanned<Expr<'src>>>),

    Rust(Rc<Box<dyn Fn(Span, Vec<Value<'src>>) -> Result<Value<'src>, Error>>>),
}

impl<'src> Value<'src> {
    pub fn type_(&self) -> Result<Type, String> {
        Ok(match self {
            Value::Null => Type::Primitive(Primitive::Null),
            Value::Bool(_) => Type::Primitive(Primitive::Bool),
            Value::Num(_) => Type::Primitive(Primitive::Num),
            Value::Str(_) => Type::Primitive(Primitive::String),
            Value::List(body) => {
                let item_type = body
                    .first()
                    .map(|x| x.type_())
                    .unwrap_or(Ok(Type::Primitive(Primitive::Null)))?;

                for item in body {
                    if item.type_()? != item_type {
                        return Err(format!(
                            "List's type is `{item_type:?}`, but found an item with type `{:?}`",
                            item.type_()
                        ));
                    }
                }

                Type::Primitive(Primitive::List(Box::new(item_type)))
            }
            Value::Func(_, _) => unreachable!("`Func` value type should not be checked!"),
            Value::Rust(_) => unreachable!("`Rust` value type should not be checked!"),
        })
    }

    pub fn idx(&self, idx: &Self) -> Result<Value<'src>, String> {
        match (self, idx) {
            (Value::List(l), Value::Num(i)) => Ok(l[*i as usize].clone()),
            _ => Err(format!(
                "Cannot index `{}` with `{}`. You can only index lists by numbers!",
                self, idx
            )),
        }
    }

    pub fn add(&self, rhs: &Self) -> Result<Value<'src>, String> {
        match (self, rhs) {
            (Value::Null, Value::Null) => Err(format!("Cannot add null values")),
            (Value::Bool(_), Value::Bool(_)) => Err(format!("Cannot add booleans")),
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
            (Value::Str(a), Value::Str(b)) => Ok(Value::Str(a.to_string() + b)),
            (Value::List(a), Value::List(b)) => {
                let mut b = b.clone();
                let mut a = a.clone();
                a.append(&mut b);
                Ok(Value::List(a))
            }
            (Value::Func(_, _), Value::Func(_, _)) => Err(format!("Cannot add functions")),
            (Value::Rust(_), Value::Rust(_)) => Err(format!("Cannot add functions")),
            _ => Err(format!(
                "Cannot add `{}` and `{}` because they aren't the same type!",
                self, rhs
            )),
        }
    }

    pub fn sub(&self, rhs: &Self) -> Result<Value<'src>, String> {
        match (self, rhs) {
            (Value::Null, Value::Null) => Err(format!("Cannot subtract null values")),
            (Value::Bool(_), Value::Bool(_)) => Err(format!("Cannot subtract booleans")),
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a - b)),
            (Value::Str(_), Value::Str(_)) => Err(format!("Cannot subtract strings")),
            (Value::List(_), Value::List(_)) => Err(format!("Cannot subtract lists")),
            (Value::Func(_, _), Value::Func(_, _)) => Err(format!("Cannot subtract functions")),
            (Value::Rust(_), Value::Rust(_)) => Err(format!("Cannot subtract functions")),
            _ => Err(format!(
                "Cannot add `{}` and `{}` because they aren't the same type!",
                self, rhs
            )),
        }
    }

    pub fn mul(&self, rhs: &Self) -> Result<Value<'src>, String> {
        match (self, rhs) {
            (Value::Null, Value::Null) => Err(format!("Cannot subtract null values")),
            (Value::Bool(_), Value::Bool(_)) => Err(format!("Cannot subtract booleans")),
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a * b)),
            (Value::Str(_), Value::Str(_)) => Err(format!("Cannot subtract strings")),
            (Value::List(_), Value::List(_)) => Err(format!("Cannot subtract lists")),
            (Value::Func(_, _), Value::Func(_, _)) => Err(format!("Cannot subtract functions")),
            (Value::Rust(_), Value::Rust(_)) => Err(format!("Cannot subtract functions")),
            _ => Err(format!(
                "Cannot add `{}` and `{}` because they aren't the same type!",
                self, rhs
            )),
        }
    }
    pub fn div(&self, rhs: &Self) -> Result<Value<'src>, String> {
        match (self, rhs) {
            (Value::Null, Value::Null) => Err(format!("Cannot subtract null values")),
            (Value::Bool(_), Value::Bool(_)) => Err(format!("Cannot subtract booleans")),
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a / b)),
            (Value::Str(_), Value::Str(_)) => Err(format!("Cannot subtract strings")),
            (Value::List(_), Value::List(_)) => Err(format!("Cannot subtract lists")),
            (Value::Func(_, _), Value::Func(_, _)) => Err(format!("Cannot subtract functions")),
            (Value::Rust(_), Value::Rust(_)) => Err(format!("Cannot subtract functions")),
            _ => Err(format!(
                "Cannot add `{}` and `{}` because they aren't the same type!",
                self, rhs
            )),
        }
    }

    pub fn neg(&self) -> Result<Value<'src>, String> {
        match self {
            Value::Num(a) => Ok(Value::Num(-a)),
            _ => Err(format!("Cannot neg `{}`, as it isn't a number!", self)),
        }
    }
    pub fn not(&self) -> Result<Value<'src>, String> {
        match self {
            Value::Bool(a) => Ok(Value::Bool(!a)),
            _ => Err(format!("Cannot invert `{}`, as it isn't a boolean!", self)),
        }
    }

    pub fn eq(&self, rhs: &Self) -> Result<bool, String> {
        match (self, rhs) {
            (Value::Null, Value::Null) => Err(format!("Cannot compare null values")),
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (Value::Num(a), Value::Num(b)) => Ok(a == b),
            (Value::Str(a), Value::Str(b)) => Ok(a == b),
            (Value::List(a), Value::List(b)) => Ok(a.iter().zip(b).all(|(a, b)| a.eq(b).unwrap())),
            (Value::Func(_, _), Value::Func(_, _)) => Err(format!("Cannot compare function types")),
            (Value::Rust(_), Value::Rust(_)) => Err(format!("Cannot compare rust function types")),
            _ => Err(format!(
                "Cannot compare `{}` and `{}` because they aren't the same type",
                self, rhs
            )),
        }
    }

    pub fn ne(&self, rhs: &Self) -> Result<bool, String> {
        Ok(!self.eq(rhs)?)
    }

    pub fn lt(&self, rhs: &Self) -> Result<bool, String> {
        match (self, rhs) {
            (Value::Null, Value::Null) => Err(format!("Cannot compare null values")),
            (Value::Bool(a), Value::Bool(b)) => Ok(a < b),
            (Value::Num(a), Value::Num(b)) => Ok(a < b),
            (Value::Str(a), Value::Str(b)) => Ok(a < b),
            (Value::List(_), Value::List(_)) => Err(format!("Cannot compare list types")),
            (Value::Func(_, _), Value::Func(_, _)) => Err(format!("Cannot compare function types")),
            (Value::Rust(_), Value::Rust(_)) => Err(format!("Cannot compare rust function types")),
            _ => Err(format!(
                "Cannot compare `{}` and `{}` because they aren't the same type",
                self, rhs
            )),
        }
    }

    pub fn lte(&self, rhs: &Self) -> Result<bool, String> {
        Ok(self.lt(rhs)? || self.eq(rhs)?)
    }

    pub fn gt(&self, rhs: &Self) -> Result<bool, String> {
        match (self, rhs) {
            (Value::Null, Value::Null) => Err(format!("Cannot compare null values")),
            (Value::Bool(a), Value::Bool(b)) => Ok(a > b),
            (Value::Num(a), Value::Num(b)) => Ok(a > b),
            (Value::Str(a), Value::Str(b)) => Ok(a > b),
            (Value::List(_), Value::List(_)) => Err(format!("Cannot compare list types")),
            (Value::Func(_, _), Value::Func(_, _)) => Err(format!("Cannot compare function types")),
            (Value::Rust(_), Value::Rust(_)) => Err(format!("Cannot compare rust function types")),
            _ => Err(format!(
                "Cannot compare `{}` and `{}` because they aren't the same type",
                self, rhs
            )),
        }
    }

    pub fn gte(&self, rhs: &Self) -> Result<bool, String> {
        Ok(self.gt(rhs)? || self.eq(rhs)?)
    }

    pub fn or(&self, rhs: &Self) -> Result<bool, String> {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) => Ok(*a || *b),
            _ => Err(format!(
                "Cannot compare `{}` and `{}` because they aren't both booleans",
                self, rhs
            )),
        }
    }
    pub fn and(&self, rhs: &Self) -> Result<bool, String> {
        match (self, rhs) {
            (Value::Bool(a), Value::Bool(b)) => Ok(*a && *b),
            _ => Err(format!(
                "Cannot compare `{}` and `{}` because they aren't both booleans",
                self, rhs
            )),
        }
    }
}

impl<'src> Display for Value<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Null => "null".to_string(),
                Self::Bool(b) => b.to_string(),
                Self::Num(n) => n.to_string(),
                Self::Str(s) => s.to_string(),
                Self::List(l) => l
                    .iter()
                    .map(|x| format!("{:?}", x))
                    .collect::<Vec<_>>()
                    .join(", "),
                Self::Func(_, _) => "function".to_string(),
                Self::Rust(_) => "rust function".to_string(),
            }
        )
    }
}
impl<'src> Debug for Value<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Null => "null".to_string(),
                Self::Bool(b) => b.to_string(),
                Self::Num(n) => n.to_string(),
                Self::Str(s) => s.to_string(),
                Self::List(l) => l
                    .iter()
                    .map(|x| format!("{:?}", x))
                    .collect::<Vec<_>>()
                    .join(", "),
                Self::Func(_, _) => "function".to_string(),
                Self::Rust(_) => "rust function".to_string(),
            }
        )
    }
}
