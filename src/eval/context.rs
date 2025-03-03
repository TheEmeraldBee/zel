use std::collections::{BTreeMap, HashMap};

use crate::semantic::{SemanticVar, TopLevel};

use super::Variable;

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
