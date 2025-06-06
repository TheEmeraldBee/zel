use std::collections::HashMap;

use thiserror::Error;

use crate::{
    ast::{expr::Expr, literal::Literal, ops::BinaryOp},
    func::FunctionScope,
    scope::{Scope, Solver, Variable, VariableValue},
    types::{Struct, Type},
    value::Value,
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

    #[error("Array expected `{0}` elements, but got `{1}`")]
    ArrayLengthInvalid(usize, usize),

    #[error("Array index invalid. Expected between `0` and `{0}`, but got `{1}`")]
    BadIndex(usize, i64),
}

/// The goal of comptime is to make a system that can execute arbitrary code at compile time
/// This will be the system for creating types like structs, as well as running debug code, like printing, or asserting things are true.
#[derive(Default)]
pub struct Comptime {
    global_funcs: HashMap<
        String,
        &'static dyn Fn(&mut Self, &mut Scope<Value>, Vec<Value>) -> Result<Value, ComptimeError>,
    >,
}

impl Comptime {
    pub fn register_func(
        &mut self,
        name: impl ToString,
        func: &'static dyn Fn(
            &mut Self,
            &mut Scope<Value>,
            Vec<Value>,
        ) -> Result<Value, ComptimeError>,
    ) {
        self.global_funcs.insert(name.to_string(), func);
    }
}

impl Solver<Value> for Comptime {
    fn solve(
        &mut self,
        scope: &mut Scope<Value>,
        funcs: &mut FunctionScope,
        expr: &Expr,
    ) -> Result<Value, ComptimeError> {
        let res = match expr {
            Expr::Literal(l) => Value::Literal(l.clone()),

            Expr::Binary { lhs, op, rhs } => {
                let lhs = self.solve(scope, funcs, lhs)?;
                let rhs = self.solve(scope, funcs, rhs)?;

                lhs.apply_op(op, &rhs)?
            }

            Expr::Let {
                mutable,
                name,
                body,
            } => {
                let body_value = self.solve(scope, funcs, body)?;
                scope.register(
                    name,
                    Variable {
                        mutable: *mutable,
                        value: VariableValue::Initialized(body_value),
                    },
                );
                Value::Null
            }

            Expr::Set { target, body } => {
                let r_value = self.solve(scope, funcs, body)?;

                let l_value_ref = self.l_solve(scope, funcs, target)?;

                if l_value_ref.type_of() != r_value.type_of() {
                    return Err(ComptimeError::TypeError(
                        l_value_ref.type_of(),
                        r_value.type_of(),
                    ));
                }

                *l_value_ref = r_value.clone();

                Value::Null
            }

            Expr::Local(v) => scope.get(&v, self, funcs)?.clone().value.solved(),

            Expr::Func {
                args,
                body,
                return_type,
            } => Value::Function(funcs.register(args.clone(), *body.clone(), *return_type.clone())),

            Expr::Call { func, args } => {
                scope.push_scope();

                let executed_args = args
                    .iter()
                    .map(|x| {
                        let value = match self.solve(scope, funcs, x) {
                            Ok(t) => t,
                            Err(e) => return Err(e),
                        };
                        Ok(value)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                'custom_func: {
                    if let Expr::Local(v) = &**func {
                        let Some(func) = self.global_funcs.get(v) else {
                            break 'custom_func;
                        };

                        let res = (*func)(self, scope, executed_args)?;

                        scope.pop_scope();

                        return Ok(res);
                    }
                };

                let Value::Function(id) = self.solve(scope, funcs, func)? else {
                    return Err(ComptimeError::NotAFunction(*func.clone()));
                };

                let ret_val = funcs.call(scope, self, id, executed_args)?;

                scope.pop_scope();

                ret_val
            }

            Expr::If { cond, body, else_ } => {
                scope.push_scope();
                let res = self.solve(scope, funcs, &cond)?;
                scope.pop_scope();

                let Value::Literal(Literal::Bool(cond)) = res else {
                    return Err(ComptimeError::Expected("bool".to_string()));
                };

                let has_else = else_.is_some();

                if cond {
                    let val = self.solve(scope, funcs, body)?;
                    if has_else { val } else { Value::Null }
                } else if let Some(else_) = else_ {
                    let val = self.solve(scope, funcs, else_)?;
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
                self.solve(scope, funcs, first)?;

                loop {
                    scope.push_scope();

                    let cond_val = self.solve(scope, funcs, cond)?;
                    let Value::Literal(Literal::Bool(cond_res)) = cond_val else {
                        return Err(ComptimeError::Expected("bool".to_string()));
                    };

                    if !cond_res {
                        scope.pop_scope();
                        break;
                    }

                    self.solve(scope, funcs, body)?;
                    self.solve(scope, funcs, each)?;

                    scope.pop_scope();
                }

                scope.pop_scope();
                Value::Null
            }

            Expr::While { cond, body } => {
                loop {
                    scope.push_scope();
                    let cond_val = self.solve(scope, funcs, cond)?;
                    let Value::Literal(Literal::Bool(cond_res)) = cond_val else {
                        return Err(ComptimeError::Expected("bool".to_string()));
                    };

                    if !cond_res {
                        scope.pop_scope();
                        break;
                    }

                    self.solve(scope, funcs, body)?;
                    scope.pop_scope();
                }
                Value::Null
            }

            Expr::Block { body } => {
                scope.push_scope();
                let res = self.solve(scope, funcs, body)?;
                scope.pop_scope();
                res
            }

            Expr::Then { first, next } => {
                self.solve(scope, funcs, first)?;
                self.solve(scope, funcs, next)?
            }

            Expr::Struct { fields } => {
                let mut fixed_fields = vec![];

                for (name, expr) in fields.iter() {
                    scope.push_scope();
                    let val = self.solve(scope, funcs, expr)?;
                    let Value::Type(type_) = val else {
                        return Err(ComptimeError::NotAType(val));
                    };
                    scope.pop_scope();

                    fixed_fields.push((name.clone(), type_));
                }

                let struct_ = Struct {
                    fields: fixed_fields,
                };

                Value::Type(Type::Struct(struct_))
            }

            Expr::InitStruct { struct_, fields } => {
                let mut solved_fields = vec![];

                let Value::Type(Type::Struct(struct_)) = self.solve(scope, funcs, struct_)? else {
                    return Err(ComptimeError::Expected("struct".to_string()));
                };

                for (name, expr) in fields.iter() {
                    let res = self.solve(scope, funcs, expr)?;

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

            Expr::Array { type_, values } => {
                let mut elements = vec![];
                let array = self.solve(scope, funcs, type_)?;
                let Value::Type(val_type) = array else {
                    return Err(ComptimeError::Expected("type".to_string()));
                };

                let item_type = match val_type.clone() {
                    Type::Array(len, item_type) => {
                        if values.len() != len {
                            return Err(ComptimeError::ArrayLengthInvalid(len, values.len()));
                        }
                        item_type
                    }
                    Type::Slice(item_type) => item_type,
                    _ => return Err(ComptimeError::Expected("array | slice".to_string())),
                };

                for expr in values {
                    let value = self.solve(scope, funcs, expr)?;
                    if value.type_of() != *item_type {
                        return Err(ComptimeError::TypeError(*item_type, value.type_of()));
                    }
                    elements.push(value);
                }

                Value::Array {
                    type_: val_type,
                    elements,
                }
            }

            Expr::Index { value, index } => {
                let array = self.solve(scope, funcs, value)?;
                let index = self.solve(scope, funcs, index)?;

                let Value::Array { type_: _, elements } = array else {
                    return Err(ComptimeError::Expected("array".to_string()));
                };

                let Value::Literal(Literal::Num(n)) = index else {
                    return Err(ComptimeError::Expected("number".to_string()));
                };

                if n < 0 || n >= elements.len() as i64 {
                    return Err(ComptimeError::BadIndex(elements.len() - 1, n));
                }

                elements[n as usize].clone()
            }

            Expr::Access { val, field } => {
                let solved_val = self.solve(scope, funcs, val)?;
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
        Ok(res)
    }

    fn l_solve<'a>(
        &mut self,
        scope: &'a mut Scope<Value>,
        funcs: &mut FunctionScope,
        expr: &Expr,
    ) -> Result<&'a mut Value, ComptimeError> {
        Ok(match expr {
            Expr::Local(l) => {
                let var = scope.get_mut(l, self, funcs)?;
                if !var.mutable {
                    return Err(ComptimeError::MutateImmutable(expr.to_string()));
                }
                var.value.as_solved()
            }
            Expr::Access { val, field } => {
                let solved_val = self.l_solve(scope, funcs, val)?;
                let Value::Struct { type_, fields } = solved_val else {
                    return Err(ComptimeError::Expected("struct".to_string()));
                };

                &mut fields
                    .iter_mut()
                    .find(|x| x.0 == *field)
                    .ok_or(ComptimeError::NoField(field.clone(), type_.clone()))?
                    .1
            }
            Expr::Index { value, index } => {
                let index = self.solve(scope, funcs, index)?;
                let array = self.l_solve(scope, funcs, value)?;

                let Value::Array { type_: _, elements } = array else {
                    return Err(ComptimeError::Expected("array".to_string()));
                };

                let Value::Literal(Literal::Num(n)) = index else {
                    return Err(ComptimeError::Expected("number".to_string()));
                };

                if n < 0 || n >= elements.len() as i64 {
                    return Err(ComptimeError::BadIndex(elements.len() - 1, n));
                }

                elements.get_mut(n as usize).unwrap()
            }
            _ => return Err(ComptimeError::MutateImmutable(expr.to_string())),
        })
    }
}
