use std::collections::HashMap;

use crate::{
    parse::{MonadicOp, SetOp},
    semantic::TopLevel,
    BinaryOp, Expr, Span, Spanned, Value,
};

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

#[derive(Debug)]
pub struct Context<'src> {
    top_level: HashMap<&'src str, Variable<'src>>,
    vars: Vec<(bool, HashMap<&'src str, Variable<'src>>)>,
    globals: HashMap<&'src str, Variable<'src>>,
}

impl<'src> Default for Context<'src> {
    fn default() -> Self {
        Self::new(HashMap::new(), HashMap::new(), false)
    }
}

impl<'src> Context<'src> {
    pub fn new(
        vars: HashMap<&'src str, Variable<'src>>,
        globals: HashMap<&'src str, Variable<'src>>,
        exclusive: bool,
    ) -> Self {
        Self {
            top_level: HashMap::new(),
            vars: vec![(exclusive, vars)],
            globals,
        }
    }

    pub fn find(&self, ident: &'src str) -> Option<&Variable<'src>> {
        self.top_level
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
    }
    pub fn find_mut(&mut self, ident: &'src str) -> Option<&mut Variable<'src>> {
        self.top_level
            .get_mut(ident)
            .or_else(|| self.globals.get_mut(ident))
            .or_else(|| {
                if self
                    .vars
                    .last()
                    .expect("Stack should have at least one scope")
                    .0
                {
                    self.vars
                        .last_mut()
                        .expect("Stack should have at least one scope")
                        .1
                        .get_mut(ident)
                } else {
                    self.vars
                        .iter_mut()
                        .rfind(|x| x.1.contains_key(ident))
                        .and_then(|x| x.1.get_mut(ident))
                }
            })
    }

    pub fn insert_top_level(&mut self, top_level: TopLevel<'src>) {
        for (key, value) in top_level.vars.into_iter() {
            self.top_level.insert(key, Variable::const_expr(value));
        }
    }

    pub fn insert_global(&mut self, ident: &'src str, val: Variable<'src>) {
        self.globals.insert(ident, val);
    }

    pub fn push_scope(&mut self, exclusive: bool) {
        self.vars.push((exclusive, HashMap::new()));
    }

    pub fn pop_scope(&mut self) {
        self.vars.pop();
    }

    pub fn push_var(&mut self, ident: &'src str, var: Variable<'src>) {
        self.vars
            .last_mut()
            .expect("Stack should have at least one scope")
            .1
            .insert(ident, var);
    }
}

#[derive(Debug, Clone)]
pub enum VariableInfo<'src> {
    Value(Value<'src>),
    Expr(Spanned<Expr<'src>>),
}

#[derive(Debug, Clone)]
pub struct Variable<'src> {
    pub mutable: bool,
    val: VariableInfo<'src>,
}

impl<'src> Variable<'src> {
    pub fn mutable(val: Value<'src>) -> Self {
        Self {
            mutable: true,
            val: VariableInfo::Value(val),
        }
    }
    pub fn immutable(val: Value<'src>) -> Self {
        Self {
            mutable: false,
            val: VariableInfo::Value(val),
        }
    }

    pub fn const_expr(expr: Spanned<Expr<'src>>) -> Self {
        Self {
            mutable: false,
            val: VariableInfo::Expr(expr),
        }
    }

    pub fn as_value(&self, context: &mut Context<'src>) -> Result<Value<'src>, Error> {
        context.push_scope(true);

        let value = match &self.val {
            VariableInfo::Value(v) => v.clone(),
            VariableInfo::Expr(expr) => eval(&expr, context)?,
        };

        context.pop_scope();

        Ok(value)
    }

    pub fn as_expr(&self) -> Result<&Spanned<Expr<'src>>, String> {
        match &self.val {
            VariableInfo::Value(_) => Err(format!(
                "Failed to find main in top_level, as it is a value and not an expression!"
            )),
            VariableInfo::Expr(e) => Ok(&e),
        }
    }
}

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

        Expr::Func(args, body) => Value::Func(args.clone(), body.clone()),

        Expr::Call(func, inputted_args) => {
            let func_val = eval(&func, context)?;
            match &func_val {
                Value::Func(args, body) => {
                    let mut arg_count = args.len();

                    let mut eval_args = vec![];
                    if !args.is_empty() && args[0] == "self" {
                        eval_args.push((args[0], Variable::immutable(func_val.clone())));
                        arg_count -= 1;
                    }

                    if inputted_args.0.len() != arg_count {
                        return Err(Error::new(expression.1, format!("Trying to call function `{:?}`, but provided `{}` arguments and expected `{}` arguments.", func.0, inputted_args.0.len(), args.len())));
                    } else {
                        let mut next_args = args
                            .iter()
                            .skip(args.len() - arg_count)
                            .zip(inputted_args.0.iter())
                            .map(|(name, arg)| {
                                if *name == "self" {
                                    Err(Error::new(expression.1, format!("Function is taking `self` as a parameter, but `self` should only be used as the first parameter for a recursive function")))
                                } else {
                                    Ok((*name, Variable::immutable(eval(arg, context)?)))
                                }
                            })
                            .collect::<Result<Vec<_>, _>>()?;
                        eval_args.append(&mut next_args);
                    };

                    context.push_scope(true);

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
                    ))
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
                    ))
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
                        ))
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
                        ))
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
