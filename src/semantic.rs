use std::collections::HashMap;

use crate::{eval::Error, parse::Expr, value::Value, Spanned};

/// Represents the top level of scope
#[derive(Default, Debug)]
pub struct TopLevel<'src> {
    pub vars: HashMap<&'src str, Spanned<Expr<'src>>>,
}

pub fn analyze_top_level<'src>(
    expr: &Spanned<Expr<'src>>,
    top_level: &mut TopLevel<'src>,
) -> Result<(), Error> {
    match &expr.0 {
        Expr::Error => unreachable!("This should already be handled"),
        Expr::Const(ident, lhs, then) => {
            top_level.vars.insert(ident, *lhs.clone());
            analyze_top_level(then, top_level)?;
        }
        Expr::Value(Value::Null) => {}
        _ => {
            return Err(Error::new(
                expr.1,
                format!(
                    "Only `const` expressions are supported at top level declarations, found `{:?}`",
                    expr.0
                ),
            ));
        }
    }

    Ok(())
}
