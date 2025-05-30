use std::fmt::Display;

use crate::types::Type;

use super::{literal::Literal, ops::BinaryOp};

#[derive(Debug, Clone, Default)]
pub enum Expr {
    /// A literal value, something consistent throughout the language
    Literal(Literal),

    /// A lhs - rhs expression
    Binary {
        lhs: Box<Self>,
        op: BinaryOp,
        rhs: Box<Self>,
    },

    /// Creation of a variable
    Let {
        mutable: bool,
        name: String,
        body: Box<Self>,
    },

    /// Setting of a variable
    Set {
        name: String,
        body: Box<Self>,
    },

    /// A local variable identifier
    Local(String),

    /// A definition of a function
    Func {
        /// args are an array of idents and types
        /// types in `zel` are comptime executable exprs,
        /// which to the AST are just exprs!
        args: Vec<(String, Self)>,
        body: Box<Self>,
        return_type: Box<Self>,
    },

    If {
        cond: Box<Self>,
        body: Box<Self>,
        else_: Option<Box<Self>>,
    },

    /// This is a c-style for loop
    /// for let mut i = 0; i < 500; i = i + 1 {}
    For {
        first: Box<Self>,
        cond: Box<Self>,
        each: Box<Self>,
        body: Box<Self>,
    },

    /// This is a while loop
    /// for i < 500 {}
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

    Access {
        val: Box<Self>,
        field: String,
    },

    /// A statically defined type, this is used for implicit return types when not defining one.
    Type(Type),

    /// The calling of a function first is the function to call
    /// Second is the args to pass to the call
    Call {
        func: Box<Self>,
        args: Vec<Self>,
    },

    /// A Differentiation of a block to help show separation when doing semantic analysis
    Block {
        body: Box<Self>,
    },

    /// The most basic of operations, simply means to run the first, then return result of second.
    /// This allows for all functions to be continued in single expression
    Then {
        first: Box<Self>,
        next: Box<Self>,
    },

    #[default]
    /// A basic expression meaning that this expression is a dummy and means nothing!
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
                Expr::Let {
                    mutable,
                    name,
                    body,
                } => format!(
                    "let_{}{name}={body}",
                    match mutable {
                        true => "mut_",
                        false => "",
                    }
                ),
                Expr::Set { name, body } => format!("{name}={body}"),
                Expr::Local(v) => format!("var_{v}"),
                Expr::Func {
                    args,
                    body,
                    return_type,
                } => format!("func({args:?}{body}->{return_type})"),
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
                Expr::Access { val, field } => format!("{val}.{field}"),
            }
        )
    }
}
