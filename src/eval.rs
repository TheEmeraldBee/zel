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

#[derive(Debug, Clone)]
pub struct Variable<'src> {
    pub mutable: bool,
    pub val: Value<'src>,
}

impl<'src> Variable<'src> {
    pub fn mutable(val: Value<'src>) -> Self {
        Self { mutable: true, val }
    }
    pub fn immutable(val: Value<'src>) -> Self {
        Self {
            mutable: false,
            val,
        }
    }
}

fn eval<'src>(
    expression: &Spanned<Expr<'src>>,
    stack: &mut Vec<(&'src str, Variable<'src>)>,
    globals: &mut HashMap<String, Variable<'src>>,
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
            // Math
            BinaryOp::Add => eval(lhs, stack, globals)?.add(&eval(rhs, stack, globals)?),
            BinaryOp::Sub => eval(lhs, stack, globals)?.sub(&eval(rhs, stack, globals)?),
            BinaryOp::Mul => eval(lhs, stack, globals)?.mul(&eval(rhs, stack, globals)?),
            BinaryOp::Div => eval(lhs, stack, globals)?.div(&eval(rhs, stack, globals)?),

            // Comparison
            BinaryOp::Eq => eval(lhs, stack, globals)?
                .eq(&eval(rhs, stack, globals)?)
                .map(Value::Bool),
            BinaryOp::NotEq => eval(lhs, stack, globals)?
                .ne(&eval(rhs, stack, globals)?)
                .map(Value::Bool),
            BinaryOp::Less => eval(lhs, stack, globals)?
                .lt(&eval(rhs, stack, globals)?)
                .map(Value::Bool),
            BinaryOp::LessEq => eval(lhs, stack, globals)?
                .lte(&eval(rhs, stack, globals)?)
                .map(Value::Bool),
            BinaryOp::Greater => eval(lhs, stack, globals)?
                .gt(&eval(rhs, stack, globals)?)
                .map(Value::Bool),
            BinaryOp::GreaterEq => eval(lhs, stack, globals)?
                .gte(&eval(rhs, stack, globals)?)
                .map(Value::Bool),

            // Boolean Checks
            BinaryOp::Or => eval(lhs, stack, globals)?
                .or(&eval(rhs, stack, globals)?)
                .map(Value::Bool),
            BinaryOp::And => eval(lhs, stack, globals)?
                .and(&eval(rhs, stack, globals)?)
                .map(Value::Bool),
        }
        .map_err(|e| Error::new(expression.1, e))?,

        Expr::Local(ident) => {
            stack
                .iter()
                .rev()
                .find(|(l, _)| *l == *ident)
                .map(|(_, v)| v.clone())
                .or_else(|| globals.get(*ident).cloned())
                .ok_or_else(|| {
                    Error::new(expression.1, format!("Variable `{}` does not exist", ident))
                })?
                .val
        }

        Expr::Var(ident, val, next) => {
            let val = eval(&val, stack, globals)?;
            stack.push((ident, Variable::mutable(val)));
            eval(&next, stack, globals)?
        }

        Expr::Const(ident, val, next) => {
            let val = eval(&val, stack, globals)?;
            stack.push((ident, Variable::immutable(val)));
            eval(&next, stack, globals)?
        }

        Expr::Set(ident, val, next) => {
            let val = eval(&val, stack, globals)?;

            let var = stack.iter_mut().find(|x| x.0 == *ident).ok_or_else(|| {
                Error::new(
                    expression.1,
                    format!("Variable `{}` not initialized!", ident),
                )
            })?;

            if !var.1.mutable {
                return Err(Error::new(
                    expression.1,
                    format!("Variable `{}` is not mutable!", ident),
                ));
            }

            var.1.val = val;

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
                            .map(|(name, arg)| {
                                Ok((*name, Variable::immutable(eval(arg, stack, globals)?)))
                            })
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

                    (**func)(expression.1, parsed_args)?
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
    globals: &mut HashMap<String, Variable<'src>>,
) -> (Result<Value<'src>, Error>, Vec<(&'src str, Variable<'src>)>) {
    let mut stack = vec![];
    (eval(&expression, &mut stack, globals), stack)
}
