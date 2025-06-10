// src/func.rs

use crate::{
    ast::expr::Expr,
    comptime::ComptimeError,
    scope::{Scope, Solver, Variable, VariableValue},
    value::Value,
};

pub type FuncId = usize;

#[derive(Clone)]
pub struct Func {
    args: Vec<(String, Expr)>,
    ret: Expr,
    body: Expr,
}

#[derive(Default)]
pub struct FunctionScope {
    funcs: Vec<Func>,
}

impl FunctionScope {
    pub fn register(&mut self, args: Vec<(String, Expr)>, body: Expr, ret: Expr) -> FuncId {
        self.funcs.push(Func { args, ret, body });
        return self.funcs.len() - 1;
    }

    // Expose the AST for the monomorphizer
    pub fn get_ast(&self, id: FuncId) -> Option<(&Vec<(String, Expr)>, &Expr, &Expr)> {
        self.funcs.get(id).map(|f| (&f.args, &f.body, &f.ret))
    }

    pub fn call(
        &mut self,
        scope: &mut Scope<Value>,
        solver: &mut impl Solver<Value>,
        id: FuncId,
        args: Vec<Value>,
    ) -> Result<Value, ComptimeError> {
        scope.push_scope();

        let func = self.funcs[id].clone();

        // Solve each argument
        for ((name, type_), val) in func.args.iter().zip(args.into_iter()) {
            let Value::Type(type_) = solver.solve(scope, self, type_)? else {
                return Err(ComptimeError::Expected("type".to_string()));
            };
            if val.type_of() != type_ {
                return Err(ComptimeError::TypeError(type_, val.type_of()));
            }

            // Register the value into scope
            scope.register(
                name,
                Variable {
                    mutable: false,
                    value: VariableValue::Initialized(val),
                },
            );
        }

        let Value::Type(ret) = solver.solve(scope, self, &func.ret)? else {
            println!("Error!");
            return Err(ComptimeError::Expected("type".to_string()));
        };

        // Solve the body
        let ret_val = solver.solve(scope, self, &func.body)?;
        if ret_val.type_of() != ret {
            return Err(ComptimeError::TypeError(ret, ret_val.type_of()));
        }

        scope.pop_scope();

        Ok(ret_val)
    }
}
