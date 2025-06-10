use std::fmt::Display;

use crate::types::Type;

use super::{literal::Literal, ops::BinaryOp};

#[derive(Debug, Clone, Default)]
pub enum Expr {
    Literal(Literal),

    Binary {
        lhs: Box<Self>,
        op: BinaryOp,
        rhs: Box<Self>,
    },

    Deref(Box<Self>),

    AddressOf(Box<Self>),

    Let {
        mutable: bool,
        name: String,
        type_annotation: Option<Box<Self>>,
        body: Box<Self>,
    },

    Set {
        target: Box<Self>,
        body: Box<Self>,
    },

    Local(String),

    Func {
        args: Vec<(String, Self)>,
        body: Box<Self>,
        return_type: Box<Self>,
    },

    Extern {
        name: String,
        args: Vec<(String, Self)>,
        return_type: Box<Self>,
    },

    If {
        cond: Box<Self>,
        body: Box<Self>,
        else_: Option<Box<Self>>,
    },

    For {
        first: Box<Self>,
        cond: Box<Self>,
        each: Box<Self>,
        body: Box<Self>,
    },

    While {
        cond: Box<Self>,
        body: Box<Self>,
    },

    Struct {
        fields: Vec<(String, Self)>,
    },

    InitStruct {
        struct_: Box<Self>,
        fields: Vec<(String, Self)>,
    },

    ArrayLiteral {
        values: Vec<Self>,
    },

    ArrayFill {
        value: Box<Self>,
        size: Box<Self>,
    },

    ArrayInit {
        type_: Box<Self>,
        values: Vec<Self>,
    },

    Index {
        value: Box<Self>,
        index: Box<Self>,
    },

    Access {
        val: Box<Self>,
        field: String,
    },

    Type(Type),

    Call {
        func: Box<Self>,
        args: Vec<Self>,
    },

    Block {
        body: Box<Self>,
    },

    Then {
        first: Box<Self>,
        next: Box<Self>,
    },

    #[default]
    Null,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Literal(l) => l.to_string(),
                Expr::Binary { lhs, op, rhs } => format!("({lhs}{op}{rhs})"),
                Expr::Deref(expr) => format!("(*{expr})"),
                Expr::AddressOf(expr) => format!("(&{expr})"),
                Expr::Let {
                    mutable,
                    name,
                    type_annotation,
                    body,
                } => format!(
                    "let {}{}{name}={body}",
                    if *mutable { "mut " } else { "" },
                    if let Some(t) = type_annotation {
                        format!(": {} ", t)
                    } else {
                        "".to_string()
                    }
                ),
                Expr::Set { target, body } => format!("{target}={body}"),
                Expr::Local(v) => format!("var_{v}"),
                Expr::Func {
                    args,
                    body,
                    return_type,
                } => format!("func({args:?}{body}->{return_type})"),
                Expr::Extern {
                    name,
                    args,
                    return_type,
                } => format!("extern_{name}({args:?}->{return_type})",),
                Expr::If { cond, body, else_ } => format!("if({cond})=>{body}|{else_:?}"),
                Expr::Call { func, args } => format!("call_{func}{args:?}"),
                Expr::Block { body } => format!("{{{body}}}"),
                Expr::Then { first, next } => format!("{first};{next}"),
                Expr::Null => "null".to_string(),
                Expr::Type(t) => format!("t_{t}"),
                Expr::For {
                    first,
                    cond,
                    each,
                    body,
                } => format!("for_({first};{cond};{each})=>{body}"),
                Expr::While { cond, body } => format!("while_({cond})=>{body}"),

                Expr::Struct { fields } => fields
                    .iter()
                    .map(|x| format!("{}: {}", x.0, x.1))
                    .reduce(|l, r| format!("{l}, {r}"))
                    .unwrap_or_default(),
                Expr::InitStruct { struct_, fields } => format!(
                    "{struct_},{}",
                    fields
                        .iter()
                        .map(|x| format!("{}: {}", x.0, x.1))
                        .reduce(|l, r| format!("{l}, {r}"))
                        .unwrap_or_default(),
                ),
                Expr::ArrayLiteral { values } => format!(
                    "[{}]",
                    values
                        .iter()
                        .map(|x| x.to_string())
                        .reduce(|l, r| format!("{l}, {r}"))
                        .unwrap_or_default()
                ),
                Expr::ArrayFill { value, size } => format!("[{}; {}]", value, size),
                Expr::ArrayInit { type_, values } => format!(
                    "{type_}[{}]",
                    values
                        .iter()
                        .map(|x| x.to_string())
                        .reduce(|l, r| format!("{l}, {r}"))
                        .unwrap_or_default()
                ),
                Expr::Index { value, index } => format!("{value}[{index}]"),
                Expr::Access { val, field } => format!("{val}.{field}"),
            }
        )
    }
}
