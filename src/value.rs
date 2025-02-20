use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{eval::Error, Expr, Span, Spanned};

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
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
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
            (Value::List(a), Value::List(b)) => Ok(a.iter().zip(b).all(|(a, b)| a.lt(b).unwrap())),
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
            (Value::List(a), Value::List(b)) => Ok(a.iter().zip(b).all(|(a, b)| a.gt(b).unwrap())),
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
