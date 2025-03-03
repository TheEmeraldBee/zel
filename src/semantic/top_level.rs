use std::collections::BTreeMap;

use crate::{Error, Spanned, parse::Expr, value::Value};

/// Represents the top level of scope
#[derive(Default, Debug)]
pub struct TopLevel<'src> {
    pub vars: BTreeMap<&'src str, Spanned<Expr<'src>>>,
}

pub fn analyze_top_level<'src>(
    expr: &Spanned<Expr<'src>>,
    top_level: &mut TopLevel<'src>,
) -> Result<(), Error> {
    match &expr.0 {
        Expr::Error => unreachable!("This should already be handled"),
        Expr::Const(ident, rhs) => {
            top_level.vars.insert(ident, *rhs.clone());
        }
        Expr::Then(a, b) => {
            analyze_top_level(a, top_level)?;
            analyze_top_level(b, top_level)?;
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
