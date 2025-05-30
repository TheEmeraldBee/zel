use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{expr::Expr, literal::Literal, ops::BinaryOp},
    scope::{Scope, Variable},
    types::{Struct, Type},
    value::{Solver, Value},
};

#[derive(Debug, Clone, Error)]
pub enum ComptimeError {
    #[error("Variable `{0}` does not exist in scope")]
    VariableMissing(String),

    #[error("Attempted to mutate immutable variable with name `{0}`")]
    MutateImmutable(String),

    #[error("Expected a function, got `{0}`")]
    NotAFunction(Expr),

    #[error("Unable to execute operator `{1}` between {0} and {2}")]
    InvalidOp(Value, BinaryOp, Value),

    #[error("Attempted to divide by zero")]
    DivisionByZero,

    #[error("Expected resulting type to be `{0}`")]
    Expected(String),

    #[error("Expected type, got `{0}`")]
    NotAType(Value),

    #[error("Expected type `{0}`, got `{1}`")]
    TypeError(Type, Type),

    #[error("Struct does not have field `{0}` with type `{1}`")]
    NoField(String, Type),
}

/// The goal of comptime is to make a system that can execute arbitrary code at compile time
/// This will be the system for creating types like structs, as well as running debug code, like printing, or asserting things are true.
#[derive(Default)]
pub struct Comptime {
    global_funcs: HashMap<
        String,
        &'static dyn Fn(&mut Self, &mut Scope, Vec<Value>) -> Result<Value, ComptimeError>,
    >,
}

impl Comptime {
    pub fn register_func(
        &mut self,
        name: impl ToString,
        func: &'static dyn Fn(&mut Self, &mut Scope, Vec<Value>) -> Result<Value, ComptimeError>,
    ) {
        self.global_funcs.insert(name.to_string(), func);
    }

    pub fn execute(
        &mut self,
        scope: &mut Scope,
        expr: &Expr,
        push_scope: bool,
    ) -> Result<Value, ComptimeError> {
        if push_scope {
            scope.push_scope();
        }
        let res = match expr {
            Expr::Literal(l) => Value::Literal(l.clone()),

            Expr::Binary { lhs, op, rhs } => {
                let lhs = self.execute(scope, lhs, false)?;
                let rhs = self.execute(scope, rhs, false)?;

                lhs.apply_op(op, &rhs)?
            }

            Expr::Let {
                mutable,
                name,
                body,
            } => {
                let body_value = self.execute(scope, body, false)?;
                scope.register(
                    name,
                    Variable {
                        mutable: *mutable,
                        value: body_value,
                    },
                );
                Value::Null
            }

            Expr::Set { name, body } => {
                let body_value = self.execute(scope, body, false)?;

                scope.set(name, body_value)?;

                Value::Null
            }

            Expr::Local(v) => scope.get(&v, self)?.clone().value,

            Expr::Func {
                args,
                body,
                return_type,
            } => {
                let mut solved_args = vec![];
                for (name, expr) in args {
                    let val = self.execute(scope, expr, true)?;
                    let Value::Type(type_) = val else {
                        return Err(ComptimeError::NotAType(val));
                    };

                    solved_args.push((name.to_string(), type_))
                }

                let ret_val = self.execute(scope, return_type, true)?;
                let Value::Type(ret_type) = ret_val else {
                    return Err(ComptimeError::NotAType(ret_val));
                };

                Value::Function {
                    args: solved_args,
                    body: *body.clone(),
                    ret_type,
                }
            }

            Expr::Call { func, args } => {
                scope.push_scope();

                let executed_args = args
                    .iter()
                    .map(|x| self.execute(scope, x, false))
                    .collect::<Result<Vec<_>, _>>()?;

                'custom_func: {
                    if let Expr::Local(v) = &**func {
                        let Some(func) = self.global_funcs.get(v) else {
                            break 'custom_func;
                        };

                        let res = (*func)(self, scope, executed_args)?;

                        if push_scope {
                            scope.pop_scope();
                        }

                        return Ok(res);
                    }
                };

                let Value::Function {
                    args: func_args,
                    mut body,
                    ret_type,
                } = self.execute(scope, func, false)?
                else {
                    return Err(ComptimeError::NotAFunction(*func.clone()));
                };

                for (arg, (name, type_)) in executed_args.into_iter().zip(func_args.iter()) {
                    if arg.type_of() != *type_ {
                        return Err(ComptimeError::TypeError(type_.clone(), arg.type_of()));
                    }
                    scope.register(
                        name,
                        Variable {
                            mutable: false,
                            value: arg,
                        },
                    );
                }

                let res = self.execute(scope, &mut body, false)?;
                if res.type_of() != ret_type {
                    return Err(ComptimeError::TypeError(res.type_of(), ret_type));
                }

                scope.pop_scope();

                res
            }

            Expr::If { cond, body, else_ } => {
                scope.push_scope();
                let res = self.execute(scope, &cond, false)?;
                scope.pop_scope();

                let Value::Literal(Literal::Bool(cond)) = res else {
                    return Err(ComptimeError::Expected("bool".to_string()));
                };

                let has_else = else_.is_some();

                if cond {
                    let val = self.execute(scope, body, true)?;
                    if has_else { val } else { Value::Null }
                } else if let Some(else_) = else_ {
                    let val = self.execute(scope, else_, true)?;
                    val
                } else {
                    Value::Null
                }
            }

            Expr::For {
                first,
                cond,
                each,
                body,
            } => {
                scope.push_scope();
                self.execute(scope, first, false)?;

                loop {
                    scope.push_scope();

                    let cond_val = self.execute(scope, cond, false)?;
                    let Value::Literal(Literal::Bool(cond_res)) = cond_val else {
                        return Err(ComptimeError::Expected("bool".to_string()));
                    };

                    if !cond_res {
                        scope.pop_scope();
                        break;
                    }

                    self.execute(scope, body, false)?;
                    self.execute(scope, each, false)?;

                    scope.pop_scope();
                }

                scope.pop_scope();
                Value::Null
            }

            Expr::While { cond, body } => {
                loop {
                    scope.push_scope();
                    let cond_val = self.execute(scope, cond, false)?;
                    let Value::Literal(Literal::Bool(cond_res)) = cond_val else {
                        return Err(ComptimeError::Expected("bool".to_string()));
                    };

                    if !cond_res {
                        scope.pop_scope();
                        break;
                    }

                    self.execute(scope, body, false)?;
                    scope.pop_scope();
                }
                Value::Null
            }

            Expr::Block { body } => self.execute(scope, body, true)?,

            Expr::Then { first, next } => {
                self.execute(scope, first, false)?;
                self.execute(scope, next, false)?
            }

            Expr::Struct { fields } => {
                let mut fixed_fields = vec![];

                for (name, expr) in fields.iter() {
                    let val = self.execute(scope, expr, true)?;
                    let Value::Type(type_) = val else {
                        return Err(ComptimeError::NotAType(val));
                    };

                    fixed_fields.push((name.clone(), type_));
                }

                let struct_ = Struct {
                    fields: fixed_fields,
                };

                Value::Type(Type::Struct(struct_))
            }

            Expr::InitStruct { struct_, fields } => {
                let mut solved_fields = vec![];

                let Value::Type(Type::Struct(struct_)) = self.execute(scope, struct_, true)? else {
                    return Err(ComptimeError::Expected("struct".to_string()));
                };

                for (name, expr) in fields.iter() {
                    let res = self.execute(scope, expr, false)?;

                    if !struct_.fields.contains(&(name.clone(), res.type_of())) {
                        return Err(ComptimeError::NoField(name.clone(), res.type_of()));
                    }

                    solved_fields.push((name.clone(), res));
                }

                Value::Struct {
                    type_: Type::Struct(struct_),
                    fields: solved_fields,
                }
            }

            Expr::Access { val, field } => {
                let solved_val = self.execute(scope, val, false)?;
                let Value::Struct { type_, fields } = solved_val else {
                    return Err(ComptimeError::Expected("struct".to_string()));
                };

                fields
                    .iter()
                    .find(|x| x.0 == *field)
                    .ok_or(ComptimeError::NoField(field.clone(), type_))?
                    .1
                    .clone()
            }

            Expr::Type(t) => Value::Type(t.clone()),

            // Do nothing in the case of a null expression
            Expr::Null => Value::Null,
        };
        if push_scope {
            scope.pop_scope();
        }
        Ok(res)
    }
}

impl Solver for Comptime {
    fn solve(&mut self, scope: &mut Scope, expr: &Expr) -> Result<Value, ComptimeError> {
        self.execute(scope, expr, false)
    }
}
