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

pub struct SemanticVar {
    pub mutable: bool,
}

impl SemanticVar {
    pub fn mutable() -> Self {
        Self { mutable: true }
    }

    pub fn immutable() -> Self {
        Self { mutable: false }
    }
}

pub struct SemanticContext<'src> {
    top_level: HashMap<&'src str, SemanticVar>,
    vars: Vec<(bool, HashMap<&'src str, SemanticVar>)>,
    globals: HashMap<&'src str, SemanticVar>,
}

impl<'src> SemanticContext<'src> {
    pub fn new(top_level: &TopLevel<'src>, globals: HashMap<&'src str, SemanticVar>) -> Self {
        let mut ctx = Self {
            top_level: HashMap::new(),
            vars: vec![(false, HashMap::new())],
            globals,
        };

        for ident in top_level.vars.keys() {
            ctx.top_level.insert(ident, SemanticVar::immutable());
        }

        ctx
    }

    pub fn register(&mut self, ident: &'src str, var: SemanticVar) {
        self.vars
            .last_mut()
            .expect("Stack should have at least one scope")
            .1
            .insert(ident, var);
    }

    pub fn has(&self, ident: &'src str, require_mut: bool) -> Result<(), String> {
        if let Some(val) = self
            .top_level
            .get(ident)
            .or_else(|| self.globals.get(ident))
            .or_else(|| {
                let vars = self
                    .vars
                    .last()
                    .expect("Stack should have at least one scope");
                if vars.0 {
                    vars.1.get(ident)
                } else {
                    self.vars
                        .iter()
                        .rfind(|x| x.1.contains_key(ident))
                        .and_then(|x| x.1.get(ident))
                }
            })
        {
            if require_mut && !val.mutable {
                Err(format!("Variable `{ident}` must be mutable but is not"))
            } else {
                Ok(())
            }
        } else {
            Err(format!("Variable `{ident}` is not available in this scope"))
        }
    }

    pub fn push_scope(&mut self, top: bool) {
        self.vars.push((top, HashMap::new()));
    }
    pub fn pop_scope(&mut self) {
        self.vars.pop();
    }
}

pub fn analyze_expr<'src>(
    expr: &Spanned<Expr<'src>>,
    ctx: &mut SemanticContext<'src>,
) -> Result<(), Error> {
    match &expr.0 {
        Expr::Error => unreachable!("This should already be handled!"),
        Expr::Value(_) => {
            // Values don't have any variables that may not exist
        }
        Expr::List(list_exprs) => {
            // Lists are built of expressions, so we need to check each item
            for list_expr in list_exprs {
                analyze_expr(list_expr, ctx)?
            }
        }

        Expr::Local(ident) => {
            // This is the big one, we need to make sure that the variable for this local exists
            ctx.has(ident, false).map_err(|e| Error::new(expr.1, e))?
        }

        Expr::Var(ident, body) => {
            // Register the variable as mutable, and then analyze the body
            ctx.register(&ident, SemanticVar::mutable());
            analyze_expr(body, ctx)?
        }
        Expr::Const(ident, body) => {
            // Register the variable as immutable, and then analyze the body
            ctx.register(&ident, SemanticVar::immutable());
            analyze_expr(body, ctx)?
        }
        Expr::Set(ident, _, body) => {
            ctx.has(ident, true).map_err(|e| Error::new(expr.1, e))?;
            analyze_expr(body, ctx)?
        }

        Expr::Then(first, last) => {
            analyze_expr(first, ctx)?;
            analyze_expr(last, ctx)?;
        }

        Expr::Block(body) => {
            ctx.push_scope(false);
            analyze_expr(body, ctx)?;
            ctx.pop_scope();
        }

        Expr::Binary(lhs, _, rhs) => {
            analyze_expr(lhs, ctx)?;
            analyze_expr(rhs, ctx)?;
        }

        Expr::Monary(body, _) => analyze_expr(body, ctx)?,

        Expr::If(cond, body, else_) => {
            ctx.push_scope(false);
            analyze_expr(cond, ctx)?;
            analyze_expr(body, ctx)?;
            analyze_expr(else_, ctx)?;
            ctx.pop_scope();
        }

        Expr::For(a, b, c, d) => {
            ctx.push_scope(false);
            analyze_expr(a, ctx)?;
            analyze_expr(b, ctx)?;
            analyze_expr(c, ctx)?;
            analyze_expr(d, ctx)?;
            ctx.pop_scope();
        }

        Expr::While(cond, body) => {
            ctx.push_scope(false);
            analyze_expr(cond, ctx)?;
            analyze_expr(body, ctx)?;
            ctx.pop_scope();
        }

        Expr::Break | Expr::Continue => {}

        Expr::Return(body) => analyze_expr(body, ctx)?,

        Expr::Call(caller, args) => {
            analyze_expr(caller, ctx)?;
            for arg in &args.0 {
                analyze_expr(arg, ctx)?;
            }
        }

        Expr::Func(args, body) => {
            ctx.push_scope(true);

            for arg in args {
                ctx.register(arg, SemanticVar::immutable());
            }

            analyze_expr(body, ctx)?;

            ctx.pop_scope();
        }
    }
    Ok(())
}
