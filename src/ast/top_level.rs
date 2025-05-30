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
pub struct TopLevel {
    exprs: Vec<(String, Expr)>,
}

impl TopLevel {
    pub fn populate(&mut self, expr: Expr) -> Result<(), TopLevelError> {
        match &expr {
            Expr::Let {
                name,
                body,
                mutable: false,
            } => self.exprs.push((name.clone(), *body.clone())),
            Expr::Then { first, next } => {
                self.populate(*first.clone())?;
                self.populate(*next.clone())?;
            }
            // Ignore null expressions
            Expr::Null => {}
            _ => {
                return Err(TopLevelError::InvalidExpr(format!(
                    "Expressions in top level can only be `let ident = <expr>`, found: {:?}",
                    expr
                )));
            }
        }
        Ok(())
    }

    pub fn get(&self, name: &str) -> Result<&Expr, TopLevelError> {
        self.exprs
            .iter()
            .find(|x| x.0 == name)
            .map(|x| &x.1)
            .ok_or_else(|| TopLevelError::NotFound(name.to_string()))
    }

    pub fn require_fn(&self, name: &str, req_args: Vec<&str>) -> Result<(), TopLevelError> {
        let expr = self.get(name)?;
        if matches!(expr, Expr::Func { args, body: _, return_type: _ } if args.iter().map(|x| &x.0).collect::<Vec<_>>() == req_args)
        {
            Ok(())
        } else {
            Err(TopLevelError::RequiredFn(name.to_string()))
        }
    }

    pub fn finish(self) -> Vec<(String, Expr)> {
        self.exprs
    }
}
