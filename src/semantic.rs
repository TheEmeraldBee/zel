use crate::{
    Error, Spanned,
    parse::{BinaryOp, Expr, MonadicOp, SetOp},
    types::{Primitive, Type},
};

pub mod top_level;
pub use top_level::*;

pub mod semantic_vars;
pub use semantic_vars::*;

pub mod context;
pub use context::*;

mod util;
use util::*;

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

        Expr::Index(ident, body) => {
            let var_type = ctx.has(ident, false).map_err(|e| Error::new(expr.1, e))?;
            let type_ = analyze_expr(body, ctx)?;

            if type_ != Type::Primitive(Primitive::Num) {
                return Err(Error::new(
                    expr.1,
                    format!("Index type must be num, found `{}`", type_),
                ));
            }

            match var_type {
                Type::Primitive(Primitive::List(l)) => *l.clone(),
                _ => {
                    return Err(Error::new(
                        expr.1,
                        format!("Type of var must be list, got `{}`", var_type),
                    ));
                }
            }
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
