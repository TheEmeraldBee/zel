use std::collections::HashMap;

use crate::{
    eval::Error,
    parse::{BinaryOp, Expr, MonadicOp, SetOp},
    types::{Primitive, Type},
    value::Value,
    Span, Spanned,
};

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

#[derive(Debug, Clone)]
pub enum TopLevelSemantic<'src> {
    Solved(SemanticVar),
    Unsolved(Spanned<Expr<'src>>),
}

impl<'src> TopLevelSemantic<'src> {
    pub fn solved(self, ctx: &mut SemanticContext<'src>) -> Result<SemanticVar, Error> {
        Ok(match self {
            Self::Solved(v) => v,
            Self::Unsolved(expr) => SemanticVar::immutable(analyze_expr(&expr, ctx)?),
        })
    }
}

#[derive(Debug, Clone)]
pub struct SemanticVar {
    pub type_: Type,
    pub mutable: bool,
}

impl SemanticVar {
    pub fn new(mutable: bool, type_: Type) -> Self {
        Self { type_, mutable }
    }
    pub fn mutable(type_: Type) -> Self {
        Self {
            type_,
            mutable: true,
        }
    }

    pub fn immutable(type_: Type) -> Self {
        Self {
            type_,
            mutable: false,
        }
    }
}

pub struct SemanticContext<'src> {
    top_level: HashMap<&'src str, TopLevelSemantic<'src>>,
    vars: Vec<(bool, HashMap<&'src str, SemanticVar>)>,
    globals: HashMap<&'src str, SemanticVar>,
}

impl<'src> SemanticContext<'src> {
    pub fn solve(
        top_level: &TopLevel<'src>,
        globals: HashMap<&'src str, SemanticVar>,
    ) -> Result<(), Error> {
        let mut ctx = Self {
            top_level: HashMap::new(),
            vars: vec![(false, HashMap::new())],
            globals,
        };

        for (ident, body) in &top_level.vars {
            // Insert top-level vars into systems.
            ctx.top_level
                .insert(ident, TopLevelSemantic::Unsolved(body.clone()));
        }

        for (ident, var) in &ctx.top_level.clone() {
            // Solve Top Level Functions
            let solved = var.clone().solved(&mut ctx)?;
            ctx.top_level
                .insert(ident, TopLevelSemantic::Solved(solved));
        }

        Ok(())
    }

    pub fn register(&mut self, ident: &'src str, var: SemanticVar) {
        self.vars
            .last_mut()
            .expect("Stack should have at least one scope")
            .1
            .insert(ident, var);
    }

    pub fn has(&mut self, ident: &'src str, require_mut: bool) -> Result<Type, String> {
        if let Some(val) = self
            .top_level
            .get(ident)
            .cloned()
            .map(|x| {
                let val = x.solved(self)?;
                self.top_level
                    .insert(ident, TopLevelSemantic::Solved(val.clone()));
                Ok(val)
            })
            .or_else(|| self.globals.get(ident).map(|x| Ok(x.clone())))
            .or_else(|| {
                let vars = self
                    .vars
                    .last()
                    .expect("Stack should have at least one scope");
                if vars.0 {
                    vars.1.get(ident).map(|x| Ok(x.clone()))
                } else {
                    self.vars
                        .iter()
                        .rfind(|x| x.1.contains_key(ident))
                        .and_then(|x| x.1.get(ident))
                        .map(|x| Ok(x.clone()))
                }
            })
        {
            let val = val.map_err(|x: Error| x.msg)?;
            if require_mut && !val.mutable {
                Err(format!("Variable `{ident}` must be mutable but is not"))
            } else {
                Ok(val.type_)
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

fn require_same(span: Span, type_a: &Type, type_b: &Type) -> Result<(), Error> {
    match type_a == type_b {
        true => Ok(()),
        false => Err(Error::new(
            span,
            format!(
                "Types for this expression need to be the same, expected `{}`, got `{}`",
                type_a, type_b
            ),
        )),
    }
}
fn require_true(span: Span, val: bool, fail_msg: impl ToString) -> Result<(), Error> {
    match val {
        true => Ok(()),
        false => Err(Error::new(span, fail_msg)),
    }
}

fn require_fn(span: Span, fn_type: &Type, args: &Vec<Type>) -> Result<Type, Error> {
    match fn_type {
        Type::Primitive(Primitive::RustFunc(expected_ret)) => {
            // require_same(span, ret_type, expected_ret)?;
            Ok(*expected_ret.clone())
        }
        Type::Primitive(Primitive::Func(arg_types, expected_ret)) => {
            for (wanted, got) in args.iter().zip(arg_types) {
                require_same(span, wanted, got)?;
            }
            // require_same(span, ret_type, expected_ret)?;
            Ok(*expected_ret.clone())
        }
        _ => Err(Error::new(
            span,
            format!(
                "Callable can only be `rustfn` or `function`, found `{:?}`",
                fn_type
            ),
        )),
    }
}

pub fn analyze_expr<'src>(
    expr: &Spanned<Expr<'src>>,
    ctx: &mut SemanticContext<'src>,
) -> Result<Type, Error> {
    Ok(match &expr.0 {
        Expr::Error => unreachable!("This should already be handled!"),
        Expr::Value(t) => {
            // Values don't have any variables that may not exist
            t.type_().map_err(|e| Error::new(expr.1, e))?
        }
        Expr::List(list_exprs) => {
            // Lists are built of expressions, so we need to check each item
            let mut type_ = Type::Primitive(Primitive::Null);
            for list_expr in list_exprs {
                let new_type = analyze_expr(list_expr, ctx)?;
                if type_ == Type::Primitive(Primitive::Null) {
                    type_ = new_type;
                }
            }
            Type::Primitive(Primitive::List(Box::new(type_)))
        }

        Expr::Local(ident) => {
            // This is the big one, we need to make sure that the variable for this local exists
            ctx.has(ident, false).map_err(|e| Error::new(expr.1, e))?
        }

        Expr::Var(ident, body) => {
            // Register the variable as mutable, and then analyze the body
            let body_type = analyze_expr(body, ctx)?;
            ctx.register(&ident, SemanticVar::mutable(body_type));
            Type::Primitive(Primitive::Null)
        }
        Expr::Const(ident, body) => {
            // Register the variable as immutable, and then analyze the body
            let body_type = analyze_expr(body, ctx)?;
            ctx.register(&ident, SemanticVar::immutable(body_type));
            Type::Primitive(Primitive::Null)
        }
        Expr::Set(ident, op, body) => {
            let var_type = ctx.has(ident, true).map_err(|e| Error::new(expr.1, e))?;
            let type_ = analyze_expr(body, ctx)?;
            require_same(expr.1, &var_type, &type_)?;

            require_true(
                expr.1,
                match op {
                    SetOp::Set => true,
                    SetOp::Add => var_type.can_add(),
                    SetOp::Sub => var_type.can_sub(),
                    SetOp::Mul => var_type.can_mul(),
                    SetOp::Div => var_type.can_div(),
                },
                format!("Operator {} is not supported for type {:?}", op, var_type),
            )?;

            Type::Primitive(Primitive::Null)
        }

        Expr::Then(first, last) => {
            analyze_expr(first, ctx)?;
            analyze_expr(last, ctx)?
        }

        Expr::Block(body) => {
            ctx.push_scope(false);
            let res = analyze_expr(body, ctx)?;
            ctx.pop_scope();
            res
        }

        Expr::Binary(lhs, op, rhs) => {
            let a_type = analyze_expr(lhs, ctx)?;
            let b_type = analyze_expr(rhs, ctx)?;
            require_same(expr.1, &a_type, &b_type)?;
            require_true(
                expr.1,
                match op {
                    BinaryOp::Add => a_type.can_add(),
                    BinaryOp::Sub => a_type.can_sub(),
                    BinaryOp::Mul => a_type.can_mul(),
                    BinaryOp::Div => a_type.can_div(),
                    BinaryOp::Eq | BinaryOp::NotEq => a_type.can_eq(),
                    BinaryOp::Greater | BinaryOp::GreaterEq | BinaryOp::Less | BinaryOp::LessEq => {
                        a_type.can_comp()
                    }
                    BinaryOp::Or | BinaryOp::And => a_type.can_bool(),
                },
                format!("Binary op {} is not supported for type of {:?}", op, a_type),
            )?;
            a_type
        }

        Expr::Monary(body, op) => {
            let type_ = analyze_expr(body, ctx)?;

            require_true(
                expr.1,
                match op {
                    MonadicOp::Neg => type_.can_neg(),
                    MonadicOp::Not => type_.can_not(),
                },
                format!(
                    "Monadic op `{}` is not supported for type of `{:?}`",
                    op, type_
                ),
            )?;

            type_
        }

        Expr::If(cond, body, else_) => {
            ctx.push_scope(false);
            analyze_expr(cond, ctx)?;
            let if_type = analyze_expr(body, ctx)?;
            let else_type = analyze_expr(else_, ctx)?;
            require_same(expr.1, &if_type, &else_type)?;
            ctx.pop_scope();
            if_type
        }

        Expr::For(a, b, c, d) => {
            ctx.push_scope(false);
            analyze_expr(a, ctx)?;
            analyze_expr(b, ctx)?;
            analyze_expr(c, ctx)?;
            analyze_expr(d, ctx)?;
            ctx.pop_scope();
            Type::Primitive(Primitive::Null)
        }

        Expr::While(cond, body) => {
            ctx.push_scope(false);
            analyze_expr(cond, ctx)?;
            analyze_expr(body, ctx)?;
            ctx.pop_scope();
            Type::Primitive(Primitive::Null)
        }

        Expr::Break | Expr::Continue => Type::Primitive(Primitive::Null),

        Expr::Return(body) => analyze_expr(body, ctx)?,

        Expr::Call(caller, args) => {
            let cal = analyze_expr(&caller, ctx)?;

            let mut arg_types = vec![];
            for arg in &args.0 {
                arg_types.push(analyze_expr(arg, ctx)?);
            }

            require_fn(expr.1, &cal, &arg_types)?
        }

        Expr::Func(args, (body, ret_type)) => {
            ctx.push_scope(true);

            let mut arg_types = vec![];

            for arg in args {
                arg_types.push(arg.1.clone());
                ctx.register(arg.0, SemanticVar::immutable(arg.1.clone()));
            }

            let this = Type::Primitive(Primitive::Func(arg_types, Box::new(ret_type.clone())));

            ctx.register("this", SemanticVar::immutable(this.clone()));

            let res = analyze_expr(body, ctx)?;
            require_same(expr.1, &ret_type, &res)?;

            ctx.pop_scope();

            this
        }
    })
}
