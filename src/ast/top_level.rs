use thiserror::Error;

use super::expr::Expr;

#[derive(Error, Debug)]
pub enum TopLevelError {
    #[error("{0}")]
    InvalidExpr(String),

    #[error("{0} was not found in top-level declarations")]
    NotFound(String),

    #[error("{0} was expected to be a function")]
    RequiredFn(String),
}

#[derive(Default)]
pub struct TopLevel<'src> {
    exprs: Vec<(&'src str, Expr<'src>)>,
}

impl<'src> TopLevel<'src> {
    pub fn populate(&mut self, expr: &Expr<'src>) -> Result<(), TopLevelError> {
        match expr {
            Expr::Then { first, next } => {
                self.populate(first)?;
                self.populate(next)
            }
            Expr::Const { name, body } => match **body {
                Expr::Func { args: _, body: _ } => {
                    self.exprs.push((name, *body.clone()));
                    Ok(())
                }
                _ => Err(TopLevelError::InvalidExpr(format!(
                    "Expressions in top level can only be `const ident = fn() {{}}`, found: {:?}",
                    expr
                ))),
            },
            _ => Err(TopLevelError::InvalidExpr(format!(
                "Expressions in top level can only be `const ident = fn() {{}}`, found: {:?}",
                expr
            ))),
        }
    }

    pub fn get(&self, name: &str) -> Result<&Expr<'src>, TopLevelError> {
        self.exprs
            .iter()
            .find(|x| x.0 == name)
            .map(|x| &x.1)
            .ok_or_else(|| TopLevelError::NotFound(name.to_string()))
    }

    pub fn require_fn(&self, name: &str, req_args: Vec<&str>) -> Result<(), TopLevelError> {
        let expr = self.get(name)?;
        if matches!(expr, Expr::Func { args, body: _ } if *args == req_args) {
            Ok(())
        } else {
            Err(TopLevelError::RequiredFn(name.to_string()))
        }
    }

    pub fn finish(self) -> Vec<(&'src str, Expr<'src>)> {
        self.exprs
    }
}
