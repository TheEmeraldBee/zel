use std::collections::BTreeMap;

use super::*;

pub struct SemanticContext<'src> {
    top_level: BTreeMap<&'src str, TopLevelSemantic<'src>>,
    vars: Vec<(bool, BTreeMap<&'src str, SemanticVar>)>,
    globals: BTreeMap<&'src str, SemanticVar>,
}

impl<'src> SemanticContext<'src> {
    pub fn solve(
        top_level: &TopLevel<'src>,
        globals: BTreeMap<&'src str, SemanticVar>,
    ) -> Result<(), Error> {
        let mut ctx = Self {
            top_level: BTreeMap::new(),
            vars: vec![(false, BTreeMap::new())],
            globals,
        };

        for (ident, body) in &top_level.vars {
            // Insert top-level vars into systems.
            ctx.top_level
                .insert(ident, TopLevelSemantic::Unsolved(body.clone()));
        }

        for (ident, var) in &ctx.top_level.clone() {
            // Solve Top Level Functions
            let solved = var.clone().solved(&mut ctx)?;
            ctx.top_level
                .insert(ident, TopLevelSemantic::Solved(solved));
        }

        Ok(())
    }

    pub fn register(&mut self, ident: &'src str, var: SemanticVar) {
        self.vars
            .last_mut()
            .expect("Stack should have at least one scope")
            .1
            .insert(ident, var);
    }

    pub fn has(&mut self, ident: &'src str, require_mut: bool) -> Result<Type, String> {
        if let Some(val) = self
            .top_level
            .get(ident)
            .cloned()
            .map(|x| {
                let val = x.solved(self)?;
                self.top_level
                    .insert(ident, TopLevelSemantic::Solved(val.clone()));
                Ok(val)
            })
            .or_else(|| self.globals.get(ident).map(|x| Ok(x.clone())))
            .or_else(|| {
                let vars = self
                    .vars
                    .last()
                    .expect("Stack should have at least one scope");
                if vars.0 {
                    vars.1.get(ident).map(|x| Ok(x.clone()))
                } else {
                    self.vars
                        .iter()
                        .rfind(|x| x.1.contains_key(ident))
                        .and_then(|x| x.1.get(ident))
                        .map(|x| Ok(x.clone()))
                }
            })
        {
            let val = val.map_err(|x: Error| x.msg)?;
            if require_mut && !val.mutable {
                Err(format!("Variable `{ident}` must be mutable but is not"))
            } else {
                Ok(val.type_)
            }
        } else {
            Err(format!("Variable `{ident}` is not available in this scope"))
        }
    }

    pub fn push_scope(&mut self, top: bool) {
        self.vars.push((top, BTreeMap::new()));
    }
    pub fn pop_scope(&mut self) {
        self.vars.pop();
    }
}
