use std::fmt::Display;

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
    Set { name: String, body: Box<Self> },

    /// A local variable identifier
    Local(String),

    /// A definition of a function
    Func {
        /// args are an array of idents and types
        /// types in `zel` are comptime executable exprs,
        /// which to the AST are just exprs!
        args: Vec<(String, Expr)>,
        body: Box<Self>,
    },

    If {
        cond: Box<Self>,
        body: Box<Self>,
        else_: Option<Box<Self>>,
    },

    /// The calling of a function first is the function to call
    /// Second is the args to pass to the call
    Call { func: Box<Self>, args: Vec<Self> },

    /// A Differentiation of a block to help show separation when doing semantic analysis
    Block { body: Box<Self> },

    /// The most basic of operations, simply means to run the first, then return result of second.
    /// This allows for all functions to be continued in single expression
    Then { first: Box<Self>, next: Box<Self> },

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
                Expr::Func { args, body } => format!("func({args:?}{body})"),
                Expr::If { cond, body, else_ } => format!("if({cond})=>{body}|{else_:?}"),
                Expr::Call { func, args } => format!("call_{func}{args:?}"),
                Expr::Block { body } => format!("{{{body}}}"),
                Expr::Then { first, next } => format!("{first};{next}"),
                Expr::Null => "null".to_string(),
            }
        )
    }
}
