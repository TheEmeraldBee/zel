use std::collections::HashMap;

use crate::{
    ast::{expr::Expr, top_level::TopLevel},
    comptime::ComptimeError,
    func::FunctionScope,
};

pub trait Solver<V: Clone> {
    fn solve(
        &mut self,
        scope: &mut Scope<V>,
        funcs: &mut FunctionScope,
        expr: &Expr,
    ) -> Result<V, ComptimeError>;
    fn l_solve<'a>(
        &mut self,
        scope: &'a mut Scope<V>,
        funcs: &mut FunctionScope,
        expr: &Expr,
    ) -> Result<&'a mut V, ComptimeError>;
}

#[derive(Debug, Clone)]
pub enum VariableValue<V: Clone> {
    Initialized(V),
    Unsolved(Expr),
}

impl<V: Clone> VariableValue<V> {
    pub fn solved(self) -> V {
        let Self::Initialized(v) = self else {
            panic!("Value should be initialized")
        };
        v
    }

    pub fn as_solved(&mut self) -> &mut V {
        let Self::Initialized(v) = self else {
            panic!("Value should be initialized")
        };
        v
    }
}

#[derive(Debug, Clone)]
pub struct Variable<V: Clone> {
    pub mutable: bool,

    pub value: VariableValue<V>,
}

pub struct Scope<V: Clone> {
    vars: Vec<HashMap<String, Variable<V>>>,
}

impl<V: Clone> Default for Scope<V> {
    /// Creates a scope with one registered scope and no variables.
    fn default() -> Self {
        Self {
            vars: vec![HashMap::default()],
        }
    }
}

impl<V: Clone> Scope<V> {
    pub fn register_top_level(&mut self, top_level: TopLevel) {
        for (name, expr) in top_level.finish() {
            self.register(
                name,
                Variable {
                    mutable: false,
                    value: VariableValue::Unsolved(expr),
                },
            );
        }
    }

    pub fn cur_scope(&mut self) -> &mut HashMap<String, Variable<V>> {
        self.vars
            .last_mut()
            .expect("There should always be one scope")
    }

    /// Register a variable in the current scope
    pub fn register(&mut self, name: impl ToString, value: Variable<V>) {
        self.cur_scope().insert(name.to_string(), value);
    }

    pub fn force_set(
        &mut self,
        name: impl AsRef<str>,
        value: VariableValue<V>,
    ) -> Result<(), ComptimeError> {
        let var = self
            .vars
            .iter_mut()
            .rfind(|x| x.contains_key(name.as_ref()))
            .ok_or(ComptimeError::VariableMissing(name.as_ref().to_string()))?
            .get_mut(name.as_ref())
            .expect("Variable should exist");

        var.value = value;

        Ok(())
    }

    pub fn set(
        &mut self,
        name: impl AsRef<str>,
        value: VariableValue<V>,
    ) -> Result<(), ComptimeError> {
        let var = self
            .vars
            .iter_mut()
            .rfind(|x| x.contains_key(name.as_ref()))
            .ok_or(ComptimeError::VariableMissing(name.as_ref().to_string()))?
            .get_mut(name.as_ref())
            .expect("Variable should exist");

        if !var.mutable {
            return Err(ComptimeError::MutateImmutable(name.as_ref().to_string()));
        }

        var.value = value;

        Ok(())
    }

    /// Retrieves a variable, searching in decending order of scopes
    /// Requires a solver in order to automatically solve unknown variables.
    /// This will allow for vars that are currently of unknown/unsolved types.
    pub fn get(
        &mut self,
        name: impl AsRef<str>,
        solver: &mut impl Solver<V>,
        funcs: &mut FunctionScope,
    ) -> Result<Variable<V>, ComptimeError> {
        let val = self
            .vars
            .iter()
            .rfind(|x| x.contains_key(name.as_ref()))
            .ok_or(ComptimeError::VariableMissing(name.as_ref().to_string()))?
            .get(name.as_ref())
            .expect("Variable should exist");

        let cloned = val.clone();

        if let VariableValue::Unsolved(expr) = cloned.value {
            let new_val = solver.solve(self, funcs, &expr)?;
            self.force_set(name, VariableValue::Initialized(new_val.clone()))?;
            Ok(Variable {
                mutable: cloned.mutable,
                value: VariableValue::Initialized(new_val),
            })
        } else {
            Ok(cloned)
        }
    }

    pub fn get_mut(
        &mut self,
        name: impl AsRef<str>,
        solver: &mut impl Solver<V>,
        funcs: &mut FunctionScope,
    ) -> Result<&mut Variable<V>, ComptimeError> {
        let val = self
            .vars
            .iter()
            .rfind(|x| x.contains_key(name.as_ref()))
            .ok_or(ComptimeError::VariableMissing(name.as_ref().to_string()))?
            .get(name.as_ref())
            .expect("Variable should exist");

        if let VariableValue::Unsolved(expr) = val.value.clone() {
            let new_val = solver.solve(self, funcs, &expr)?;
            self.force_set(name.as_ref(), VariableValue::Initialized(new_val.clone()))?;
            self.get_mut(name, solver, funcs)
        } else {
            Ok(self
                .vars
                .iter_mut()
                .rfind(|x| x.contains_key(name.as_ref()))
                .ok_or(ComptimeError::VariableMissing(name.as_ref().to_string()))?
                .get_mut(name.as_ref())
                .expect("Variable should exist"))
        }
    }

    /// Register a new scope that will shadow variables and not replace them
    pub fn push_scope(&mut self) {
        self.vars.push(HashMap::default());
    }

    /// Remove a layer of scope, forgetting about all internal values.
    pub fn pop_scope(&mut self) {
        self.vars.pop();
    }
}
