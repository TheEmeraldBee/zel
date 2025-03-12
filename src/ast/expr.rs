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

    /// A comptime executable expression
    Const { name: String, body: Box<Self> },

    /// Creation of a variable
    Let {
        mutable: bool,
        name: String,
        body: Box<Self>,
    },

    /// A local variable identifier
    Local(String),

    /// A definition of a function
    Func { args: Vec<String>, body: Box<Self> },

    /// The calling of a function first is the function to call
    /// Second is the args to pass to the call
    Call { func: Box<Self>, args: Vec<Self> },

    /// The most basic of operations, simply means to run the first, then return result of second.
    /// This allows for all functions to be continued in single expression
    Then { first: Box<Self>, next: Box<Self> },

    #[default]
    /// A basic expression meaning that this expression is a dummy and means nothing!
    Null,
}
