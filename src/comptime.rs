// src/comptime.rs

use std::{collections::HashMap, fs, path::PathBuf};

use thiserror::Error;

use crate::{
    ast::{expr::Expr, literal::Literal, ops::BinaryOp, top_level::TopLevel},
    func::{FuncId, FunctionScope},
    lexer::Lexer,
    parser::Parser,
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

    #[error("Unable to solve `{0}` at comptime")]
    Unsolvable(Expr),

    #[error("Module error: {0}")]
    ModuleError(String),
}

pub struct Comptime {
    global_funcs: HashMap<
        String,
        &'static dyn Fn(&mut Self, &mut Scope<Value>, Vec<Value>) -> Result<Value, ComptimeError>,
    >,
    pub file_stack: Vec<PathBuf>,
}

impl Comptime {
    pub fn new(initial_path: PathBuf) -> Self {
        Self {
            global_funcs: HashMap::default(),
            file_stack: vec![initial_path],
        }
    }

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

    pub fn is_comptime_fn(&self, name: &str) -> bool {
        self.global_funcs.contains_key(name)
    }

    pub fn call_comptime_fn(
        &mut self,
        name: &str,
        scope: &mut Scope<Value>,
        args: Vec<Value>,
    ) -> Result<Value, ComptimeError> {
        let func = self.global_funcs.get(name).unwrap();
        (*func)(self, scope, args)
    }

    pub fn get_fn_id(
        &mut self,
        name: &str,
        scope: &mut Scope<Value>,
    ) -> Result<Option<FuncId>, ComptimeError> {
        let var = self.solve(
            scope,
            &mut FunctionScope::default(),
            &Expr::Local(name.to_string()),
        )?;
        if let Value::Function(id) = var {
            Ok(Some(id))
        } else {
            Ok(None)
        }
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
            Expr::Extern { .. } => return Err(ComptimeError::Unsolvable(expr.clone())),
            Expr::AddressOf(_) => return Err(ComptimeError::Unsolvable(expr.clone())),

            Expr::Deref(i) => {
                // Derefing a type is actually a pointer
                let type_ = self.solve(scope, funcs, i)?;
                let Value::Type(t) = type_ else {
                    return Err(ComptimeError::NotAType(type_));
                };

                Value::Type(Type::Pointer(Box::new(t)))
            }

            Expr::Literal(l) => Value::Literal(l.clone()),

            Expr::Binary { lhs, op, rhs } => {
                let lhs = self.solve(scope, funcs, lhs)?;
                let rhs = self.solve(scope, funcs, rhs)?;

                lhs.apply_op(op, &rhs)?
            }

            Expr::Let {
                mutable,
                name,
                type_annotation,
                body,
            } => {
                let body_value = self.solve(scope, funcs, body)?;

                if let Some(type_expr) = type_annotation {
                    let type_val = self.solve(scope, funcs, type_expr)?;
                    let Value::Type(t) = type_val else {
                        return Err(ComptimeError::Expected("a type".to_string()));
                    };
                    if body_value.type_of() != t {
                        return Err(ComptimeError::TypeError(t, body_value.type_of()));
                    }
                }

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

                // Handle `import` as a special comptime form
                if let Expr::Local(name) = &**func {
                    if name == "import" {
                        if args.len() != 1 {
                            return Err(ComptimeError::Expected(
                                "1 argument for import".to_string(),
                            ));
                        }
                        let path_val = self.solve(scope, funcs, &args[0])?;
                        let Value::Literal(Literal::String(relative_path)) = path_val else {
                            return Err(ComptimeError::Expected(
                                "a string literal for import path".to_string(),
                            ));
                        };

                        let current_dir = self.file_stack.last().unwrap();
                        let full_path = current_dir.join(relative_path);

                        let src = fs::read_to_string(&full_path).map_err(|e| {
                            ComptimeError::ModuleError(format!(
                                "Could not read file {}: {}",
                                full_path.display(),
                                e
                            ))
                        })?;

                        let new_dir = full_path.parent().unwrap().to_path_buf();
                        self.file_stack.push(new_dir);

                        let tokens = Lexer::lex(&src)
                            .map_err(|e| ComptimeError::ModuleError(e.to_string()))?;
                        let ast = Parser::parse(tokens)
                            .map_err(|e| ComptimeError::ModuleError(e.to_string()))?;

                        let mut top_level = TopLevel::default();
                        top_level
                            .populate(ast)
                            .map_err(|e| ComptimeError::ModuleError(e.to_string()))?;

                        let mut fields = vec![];
                        let mut type_fields = vec![];

                        for (name, expr) in top_level.iter() {
                            let value = self.solve(scope, funcs, expr)?;
                            type_fields.push((name.clone(), value.type_of()));
                            fields.push((name.clone(), value));
                        }

                        self.file_stack.pop();

                        let struct_type = Type::Struct(Struct {
                            fields: type_fields,
                        });
                        return Ok(Value::Struct {
                            type_: struct_type,
                            fields,
                        });
                    }
                }

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

                {
                    if let Expr::Local(v) = &**func {
                        if let Some(func) = self.global_funcs.get(v) {
                            let res = (*func)(self, scope, executed_args)?;
                            scope.pop_scope();
                            return Ok(res);
                        }
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

            Expr::ArrayLiteral { values } => {
                let elements: Vec<_> = values
                    .iter()
                    .map(|v| self.solve(scope, funcs, v))
                    .collect::<Result<_, _>>()?;
                let elem_type = elements[0].type_of();
                let array_type = Type::Array(elements.len(), Box::new(elem_type));
                Value::Array {
                    type_: array_type,
                    elements,
                }
            }

            Expr::ArrayFill { value, size } => {
                let elem_val = self.solve(scope, funcs, value)?;
                let size_val = self.solve(scope, funcs, size)?;
                let Value::Literal(Literal::Num(n)) = size_val else {
                    return Err(ComptimeError::Expected(
                        "an integer for array size".to_string(),
                    ));
                };
                let elements = vec![elem_val.clone(); n as usize];
                let array_type = Type::Array(n as usize, Box::new(elem_val.type_of()));
                Value::Array {
                    type_: array_type,
                    elements,
                }
            }

            Expr::ArrayInit { type_, values } => {
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
                    Type::Pointer(item_type) => item_type,
                    _ => return Err(ComptimeError::Expected("array | pointer".to_string())),
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
            _ => return Err(ComptimeError::Unsolvable(expr.clone())),
        })
    }
}
