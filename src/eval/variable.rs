use crate::{Spanned, parse::Expr, value::Value};

use super::{Context, Error, eval};

#[derive(Debug, Clone)]
pub enum VariableInfo<'src> {
    Value(Value<'src>),
    Expr(Spanned<Expr<'src>>),
}

#[derive(Debug, Clone)]
pub struct Variable<'src> {
    pub mutable: bool,
    val: VariableInfo<'src>,
}

impl<'src> Variable<'src> {
    pub fn mutable(val: Value<'src>) -> Self {
        Self {
            mutable: true,
            val: VariableInfo::Value(val),
        }
    }
    pub fn immutable(val: Value<'src>) -> Self {
        Self {
            mutable: false,
            val: VariableInfo::Value(val),
        }
    }

    pub fn const_expr(expr: Spanned<Expr<'src>>) -> Self {
        Self {
            mutable: false,
            val: VariableInfo::Expr(expr),
        }
    }

    pub fn as_value(&self, context: &mut Context<'src>) -> Result<Value<'src>, Error> {
        context.push_scope(true);

        let value = match &self.val {
            VariableInfo::Value(v) => v.clone(),
            VariableInfo::Expr(expr) => eval(&expr, context)?,
        };

        context.pop_scope();

        Ok(value)
    }

    pub fn as_expr(&self) -> Result<&Spanned<Expr<'src>>, String> {
        match &self.val {
            VariableInfo::Value(_) => Err(format!(
                "Failed to find main in top_level, as it is a value and not an expression!"
            )),
            VariableInfo::Expr(e) => Ok(&e),
        }
    }
}
