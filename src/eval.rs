use crate::{
    Error, Spanned,
    parse::{BinaryOp, Expr, MonadicOp, SetOp},
    value::Value,
};

pub mod variable;
pub use variable::*;

pub mod context;
pub use context::*;

pub fn eval<'src>(
    expression: &Spanned<Expr<'src>>,
    context: &mut Context<'src>,
) -> Result<Value<'src>, Error> {
    Ok(match &expression.0 {
        Expr::Error => unreachable!("This cannot exist in valid ast"),
        Expr::Value(v) => v.clone(),
        Expr::List(items) => Value::List(
            items
                .iter()
                .map(|exp| eval(exp, context))
                .collect::<Result<_, _>>()?,
        ),

        Expr::Binary(lhs, op, rhs) => match op {
            // Math
            BinaryOp::Add => eval(lhs, context)?.add(&eval(rhs, context)?),
            BinaryOp::Sub => eval(lhs, context)?.sub(&eval(rhs, context)?),
            BinaryOp::Mul => eval(lhs, context)?.mul(&eval(rhs, context)?),
            BinaryOp::Div => eval(lhs, context)?.div(&eval(rhs, context)?),

            // Comparison
            BinaryOp::Eq => eval(lhs, context)?
                .eq(&eval(rhs, context)?)
                .map(Value::Bool),
            BinaryOp::NotEq => eval(lhs, context)?
                .ne(&eval(rhs, context)?)
                .map(Value::Bool),
            BinaryOp::Less => eval(lhs, context)?
                .lt(&eval(rhs, context)?)
                .map(Value::Bool),
            BinaryOp::LessEq => eval(lhs, context)?
                .lte(&eval(rhs, context)?)
                .map(Value::Bool),
            BinaryOp::Greater => eval(lhs, context)?
                .gt(&eval(rhs, context)?)
                .map(Value::Bool),
            BinaryOp::GreaterEq => eval(lhs, context)?
                .gte(&eval(rhs, context)?)
                .map(Value::Bool),

            // Boolean Checks
            BinaryOp::Or => eval(lhs, context)?
                .or(&eval(rhs, context)?)
                .map(Value::Bool),
            BinaryOp::And => eval(lhs, context)?
                .and(&eval(rhs, context)?)
                .map(Value::Bool),
        }
        .map_err(|e| Error::new(expression.1, e))?,

        Expr::Monary(expr, op) => match op {
            MonadicOp::Neg => eval(expr, context)?.neg(),
            MonadicOp::Not => eval(expr, context)?.not(),
        }
        .map_err(|e| Error::new(expression.1, e))?,

        Expr::Local(ident) => context
            .find(ident)
            .ok_or_else(|| {
                Error::new(expression.1, format!("Variable `{}` does not exist", ident))
            })?
            .clone()
            .as_value(context)?,

        Expr::Var(ident, val) => {
            let val = eval(&val, context)?;
            context.push_var(ident, Variable::mutable(val));
            Value::Null
        }

        Expr::Const(ident, val) => {
            let val = eval(&val, context)?;
            context.push_var(ident, Variable::immutable(val));
            Value::Null
        }

        Expr::Set(ident, op, val) => {
            let val = eval(&val, context)?;

            let var = context
                .find(ident)
                .ok_or_else(|| {
                    Error::new(
                        expression.1,
                        format!("Variable `{}` not initialized!", ident),
                    )
                })?
                .clone();

            if !var.mutable {
                return Err(Error::new(
                    expression.1,
                    format!("Variable `{}` is not mutable!", ident),
                ));
            }

            let new_var = Variable::mutable(
                match op {
                    SetOp::Set => Ok(val),
                    SetOp::Add => var.as_value(context)?.add(&val),
                    SetOp::Sub => var.as_value(context)?.sub(&val),
                    SetOp::Mul => var.as_value(context)?.mul(&val),
                    SetOp::Div => var.as_value(context)?.div(&val),
                }
                .map_err(|e| Error::new(expression.1, e))?,
            );

            *context.find_mut(ident).ok_or_else(|| {
                Error::new(
                    expression.1,
                    format!("Variable `{}` not initialized!", ident),
                )
            })? = new_var;

            Value::Null
        }

        Expr::Then(a, b) => {
            eval(&a, context)?;
            eval(&b, context)?
        }

        Expr::Block(a) => {
            context.push_scope(false);
            let res = eval(&a, context)?;
            context.pop_scope();
            res
        }

        Expr::Func(args, (body, _)) => Value::Func(
            args.clone().into_iter().map(|x| x.0).collect(),
            body.clone(),
        ),

        Expr::Call(func, inputted_args) => {
            let func_val = eval(&func, context)?;
            match &func_val {
                Value::Func(args, body) => {
                    let mut eval_args = vec![];

                    if inputted_args.0.len() != args.len() {
                        return Err(Error::new(
                            expression.1,
                            format!(
                                "Trying to call function `{:?}`, but provided `{}` arguments and expected `{}` arguments.",
                                func.0,
                                inputted_args.0.len(),
                                args.len()
                            ),
                        ));
                    } else {
                        let mut next_args = args
                            .iter()
                            .zip(inputted_args.0.iter())
                            .map(|(name, arg)| {
                                Ok((*name, Variable::immutable(eval(arg, context)?)))
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        eval_args.append(&mut next_args);
                    };

                    context.push_scope(true);

                    context.push_var("this", Variable::immutable(func_val.clone()));

                    for (name, val) in eval_args {
                        context.push_var(name, val);
                    }

                    let val = eval(&body, context)?;

                    context.pop_scope();

                    val
                }
                Value::Rust(func) => {
                    let parsed_args = inputted_args
                        .0
                        .iter()
                        .map(|arg| Ok(eval(arg, context)?))
                        .collect::<Result<_, _>>()?;

                    (**func)(expression.1, parsed_args)?
                }
                f => {
                    return Err(Error::new(
                        expression.1,
                        format!("Trying to call `{:?}`, which is not a function!", f),
                    ));
                }
            }
        }

        Expr::If(cond, a, b) => {
            let c = eval(cond, context)?;
            context.push_scope(false);
            let res = match c {
                Value::Bool(true) => eval(a, context)?,
                Value::Bool(false) => eval(b, context)?,
                c => {
                    return Err(Error::new(
                        expression.1,
                        format!("Conditions must result in booleans, found `{c}`"),
                    ));
                }
            };
            context.pop_scope();
            res
        }

        Expr::While(cond, a) => {
            loop {
                let c = eval(cond, context)?;
                context.push_scope(false);
                match c {
                    Value::Bool(true) => {
                        eval(a, context)?;
                    }
                    Value::Bool(false) => break,
                    c => {
                        return Err(Error::new(
                            expression.1,
                            format!("Conditions must result in booleans, found `{c}`"),
                        ));
                    }
                }
                context.pop_scope();
            }
            Value::Null
        }

        Expr::For(init, cond, each, body) => {
            context.push_scope(false);

            eval(init, context)?;

            loop {
                let c = eval(cond, context)?;
                context.push_scope(false);
                match c {
                    Value::Bool(true) => {
                        eval(body, context)?;
                    }
                    Value::Bool(false) => break,
                    c => {
                        return Err(Error::new(
                            expression.1,
                            format!("Conditions must result in booleans, found `{c}`"),
                        ));
                    }
                }
                eval(each, context)?;
                context.pop_scope();
            }

            context.pop_scope();
            Value::Null
        }

        Expr::Continue => Value::Null,
        Expr::Break => Value::Null,
        Expr::Return(v) => eval(v, context)?,
    })
}
