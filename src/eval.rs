use std::collections::HashMap;

use crate::{BinaryOp, Expr, Span, Spanned, Value};

pub struct Error {
    pub span: Span,
    pub msg: String,
}

impl Error {
    pub fn new(span: Span, msg: impl ToString) -> Self {
        Self {
            span,
            msg: msg.to_string(),
        }
    }
}

fn eval<'src>(
    expression: &Spanned<Expr<'src>>,
    stack: &mut Vec<(&'src str, Value<'src>)>,
    globals: &HashMap<String, Value<'src>>,
) -> Result<Value<'src>, Error> {
    Ok(match &expression.0 {
        Expr::Error => unreachable!("This cannot exist in valid ast"),
        Expr::Value(v) => v.clone(),
        Expr::List(items) => Value::List(
            items
                .iter()
                .map(|exp| eval(exp, stack, globals))
                .collect::<Result<_, _>>()?,
        ),

        Expr::Binary(lhs, op, rhs) => match op {
            BinaryOp::Add => eval(lhs, stack, globals)?.add(&eval(rhs, stack, globals)?),
            BinaryOp::Sub => eval(lhs, stack, globals)?.sub(&eval(rhs, stack, globals)?),
            BinaryOp::Mul => eval(lhs, stack, globals)?.mul(&eval(rhs, stack, globals)?),
            BinaryOp::Div => eval(lhs, stack, globals)?.div(&eval(rhs, stack, globals)?),
            BinaryOp::Eq => eval(lhs, stack, globals)?
                .eq(&eval(rhs, stack, globals)?)
                .map(|x| Value::Bool(x)),
            BinaryOp::NotEq => eval(lhs, stack, globals)?
                .ne(&eval(rhs, stack, globals)?)
                .map(|x| Value::Bool(x)),
            BinaryOp::Less => eval(lhs, stack, globals)?
                .lt(&eval(rhs, stack, globals)?)
                .map(|x| Value::Bool(x)),
            BinaryOp::LessEq => eval(lhs, stack, globals)?
                .lte(&eval(rhs, stack, globals)?)
                .map(|x| Value::Bool(x)),
            BinaryOp::Greater => eval(lhs, stack, globals)?
                .gt(&eval(rhs, stack, globals)?)
                .map(|x| Value::Bool(x)),
            BinaryOp::GreaterEq => eval(lhs, stack, globals)?
                .gte(&eval(rhs, stack, globals)?)
                .map(|x| Value::Bool(x)),
        }
        .map_err(|e| Error::new(expression.1, e))?,

        Expr::Local(ident) => stack
            .iter()
            .rev()
            .find(|(l, _)| *l == *ident)
            .map(|(_, v)| v.clone())
            .or_else(|| globals.get(*ident).cloned())
            .ok_or_else(|| {
                Error::new(expression.1, format!("Variable `{}` does not exist", ident))
            })?,

        Expr::Const(ident, val, next) | Expr::Var(ident, val, next) => {
            let val = eval(&val, stack, globals)?;
            stack.push((ident, val));
            eval(&next, stack, globals)?
        }

        Expr::Then(a, b) => {
            eval(&a, stack, globals)?;
            eval(&b, stack, globals)?
        }

        Expr::Func(args, body) => Value::Func(args.clone(), body.clone()),

        Expr::Call(func, inputted_args) => {
            let func_val = eval(&func, stack, globals)?;
            match func_val {
                Value::Func(args, body) => {
                    let mut stack = if inputted_args.0.len() != args.len() {
                        return Err(Error::new(expression.1, format!("Trying to call function `{:?}`, but provided `{}` arguments and expected `{}` arguments.", func.0, inputted_args.0.len(), args.len())));
                    } else {
                        args.iter()
                            .zip(inputted_args.0.iter())
                            .map(|(name, arg)| Ok((*name, eval(arg, stack, globals)?)))
                            .collect::<Result<_, _>>()?
                    };

                    eval(&body, &mut stack, globals)?
                }
                Value::Rust(func) => {
                    let parsed_args = inputted_args
                        .0
                        .iter()
                        .map(|arg| Ok(eval(arg, stack, globals)?))
                        .collect::<Result<_, _>>()?;

                    (**func)(parsed_args)
                }
                f => {
                    return Err(Error::new(
                        expression.1,
                        format!("Trying to call `{:?}`, which is not a function!", f),
                    ))
                }
            }
        }

        Expr::If(cond, a, b) => {
            let c = eval(cond, stack, globals)?;
            match c {
                Value::Bool(true) => eval(a, stack, globals)?,
                Value::Bool(false) => eval(b, stack, globals)?,
                c => {
                    return Err(Error::new(
                        expression.1,
                        format!("Conditions must result in booleans, found `{c}`"),
                    ))
                }
            }
        }
    })
}

pub fn eval_expr<'src>(
    expression: Spanned<Expr<'src>>,
    globals: HashMap<String, Value<'src>>,
) -> Result<(), Error> {
    let mut stack = vec![];
    let result = eval(&expression, &mut stack, &globals)?;
    println!(
        "{:?} was resulting value\n\n{:?} was the resulting stack",
        result, stack
    );
    Ok(())
}
