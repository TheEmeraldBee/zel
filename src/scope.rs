use std::collections::HashMap;

use crate::{
    ast::top_level::TopLevel,
    comptime::ComptimeError,
    value::{Solver, Value},
};

#[derive(Debug, Clone)]
pub struct Variable {
    pub mutable: bool,

    pub value: Value,
}

pub struct Scope {
    vars: Vec<HashMap<String, Variable>>,
}

impl Default for Scope {
    /// Creates a scope with one registered scope and no variables.
    fn default() -> Self {
        Self {
            vars: vec![HashMap::default()],
        }
    }
}

impl Scope {
    pub fn register_top_level(&mut self, top_level: TopLevel) {
        for (name, expr) in top_level.finish() {
            self.register(
                name,
                Variable {
                    mutable: false,
                    value: Value::Unsolved(expr),
                },
            );
        }
    }

    pub fn cur_scope(&mut self) -> &mut HashMap<String, Variable> {
        self.vars
            .last_mut()
            .expect("There should always be one scope")
    }

    /// Register a variable in the current scope
    pub fn register(&mut self, name: impl ToString, value: Variable) {
        self.cur_scope().insert(name.to_string(), value);
    }

    pub fn force_set(&mut self, name: impl AsRef<str>, value: Value) -> Result<(), ComptimeError> {
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

    pub fn set(&mut self, name: impl AsRef<str>, value: Value) -> Result<(), ComptimeError> {
        let var = self
            .vars
            .iter_mut()
            .rfind(|x| x.contains_key(name.as_ref()))
            .ok_or(ComptimeError::VariableMissing(name.as_ref().to_string()))?
            .get_mut(name.as_ref())
            .expect("Variable should exist");

        if value.type_of() != var.value.type_of() {
            return Err(ComptimeError::TypeError(
                var.value.type_of(),
                value.type_of(),
            ));
        }

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
        solver: &mut impl Solver,
    ) -> Result<Variable, ComptimeError> {
        let val = self
            .vars
            .iter()
            .rfind(|x| x.contains_key(name.as_ref()))
            .ok_or(ComptimeError::VariableMissing(name.as_ref().to_string()))?
            .get(name.as_ref())
            .expect("Variable should exist");

        let cloned = val.clone();

        if let Value::Unsolved(expr) = cloned.value {
            let new_val = solver.solve(self, &expr)?;
            self.force_set(name, new_val.clone())?;
            Ok(Variable {
                mutable: cloned.mutable,
                value: new_val,
            })
        } else {
            Ok(cloned)
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
