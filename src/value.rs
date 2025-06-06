use std::fmt::Display;

use crate::{
    ast::{literal::Literal, ops::BinaryOp},
    comptime::ComptimeError,
    func::FuncId,
    types::Type,
};

#[derive(Debug, Clone)]
pub enum Value {
    Literal(Literal),
    Function(FuncId),
    Struct {
        type_: Type,
        fields: Vec<(String, Value)>,
    },
    Array {
        type_: Type,
        elements: Vec<Self>,
    },
    Type(Type),
    Null,
}

impl Value {
    pub fn type_of(&self) -> Type {
        match self {
            Self::Literal(l) => match l {
                Literal::Num(_) => Type::Integer(64),
                Literal::Bool(_) => Type::Bool,
                Literal::String(_) => Type::String,
            },
            Self::Function(_) => return Type::Func,
            Self::Array { type_, .. } => type_.clone(),
            Self::Struct { type_, fields: _ } => type_.clone(),
            Self::Type(_) => Type::Type,
            Self::Null => Type::Null,
        }
    }

    pub fn apply_op(&self, op: &BinaryOp, rhs: &Value) -> Result<Value, ComptimeError> {
        match (self, rhs) {
            (Value::Literal(lhs_lit), Value::Literal(rhs_lit)) => {
                match (lhs_lit, rhs_lit) {
                    (Literal::Num(l), Literal::Num(r)) => {
                        let result = match op {
                            BinaryOp::Add => Value::Literal(Literal::Num(l + r)),
                            BinaryOp::Sub => Value::Literal(Literal::Num(l - r)),
                            BinaryOp::Mul => Value::Literal(Literal::Num(l * r)),
                            BinaryOp::Div => {
                                if *r == 0 {
                                    return Err(ComptimeError::DivisionByZero);
                                }
                                Value::Literal(Literal::Num(l / r))
                            }
                            BinaryOp::Eq => Value::Literal(Literal::Bool(l == r)),
                            BinaryOp::Ne => Value::Literal(Literal::Bool(l != r)),
                            BinaryOp::Lt => Value::Literal(Literal::Bool(l < r)),
                            BinaryOp::Lte => Value::Literal(Literal::Bool(l <= r)),
                            BinaryOp::Gt => Value::Literal(Literal::Bool(l > r)),
                            BinaryOp::Gte => Value::Literal(Literal::Bool(l >= r)),
                        };
                        Ok(result)
                    }
                    (Literal::String(a), Literal::String(b)) => {
                        let result = match op {
                            BinaryOp::Add => Value::Literal(Literal::String(format!("{a}{b}"))),
                            BinaryOp::Eq => Value::Literal(Literal::Bool(a == b)),
                            BinaryOp::Ne => Value::Literal(Literal::Bool(a != b)),
                            _ => {
                                return Err(ComptimeError::InvalidOp(
                                    self.clone(),
                                    op.clone(),
                                    rhs.clone(),
                                ));
                            }
                        };
                        Ok(result)
                    }
                    (Literal::Bool(l), Literal::Bool(r)) => {
                        let result = match op {
                            BinaryOp::Eq => Value::Literal(Literal::Bool(l == r)),
                            BinaryOp::Ne => Value::Literal(Literal::Bool(l != r)),
                            _ => {
                                return Err(ComptimeError::InvalidOp(
                                    self.clone(),
                                    op.clone(),
                                    rhs.clone(),
                                ));
                            }
                        };
                        Ok(result)
                    }
                    // Handle other literal type combinations, or error if not supported
                    _ => {
                        return Err(ComptimeError::InvalidOp(
                            self.clone(),
                            op.clone(),
                            rhs.clone(),
                        ));
                    }
                }
            }
            // If either operand is not a literal, or it's an unsolved expression
            _ => {
                return Err(ComptimeError::InvalidOp(
                    self.clone(),
                    op.clone(),
                    rhs.clone(),
                ));
            }
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Literal(l) => l.to_string(),
                Self::Function(id) => format!("func_{id}"),
                Self::Struct { type_, fields } => format!(
                    "({})[{}]",
                    type_,
                    fields
                        .iter()
                        .map(|x| format!("{}: {}", x.0, x.1))
                        .reduce(|l, r| format!("{l}, {r}"))
                        .unwrap_or_default()
                ),
                Self::Array { type_, elements } => format!(
                    "{type_}[{}]",
                    elements
                        .iter()
                        .map(|x| x.to_string())
                        .reduce(|l, r| format!("{l}, {r}"))
                        .unwrap_or_default()
                ),
                Self::Null => "null".to_string(),
                Self::Type(t) => format!("{t}"),
            }
        )
    }
}
