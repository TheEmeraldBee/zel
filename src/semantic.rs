use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use thiserror::Error;

use crate::{
    ast::{
        expr::Expr,
        literal::{Literal, min_bits},
        ops::BinaryOp,
    },
    comptime::{Comptime, ComptimeError},
    types::{CompType, IntegerType},
};

#[derive(Debug, Error)]
pub enum SemanticError {
    #[error("Var `{0}` not in scope")]
    MissingVar(String),
    #[error("Expected type `{0}`, found `{1}`")]
    ExpectedType(CompType, CompType),

    #[error("Attempting to mutate variable `{0}`, which is immutable")]
    MutateImmutable(String),

    #[error("The type of variable `{0}` was attempted to be retrieved, but is unknown")]
    UnknownType(String),

    #[error(transparent)]
    Comptime(#[from] ComptimeError),
}

#[derive(Clone, Debug)]
pub enum SemanticState {
    Solved(CompType),
    Unsolved(Expr),
}

pub struct ScopeStorage<T> {
    pub scopes: Vec<HashMap<String, (bool, T)>>,
}

impl<T> Default for ScopeStorage<T> {
    fn default() -> Self {
        Self {
            scopes: vec![HashMap::default()],
        }
    }
}

impl<T> ScopeStorage<T> {
    pub fn register(&mut self, name: String, mutable: bool, value: T) {
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name, (mutable, value));
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop_if(|a| a.len() > 1);
    }
}

pub struct SemanticSolver {
    pub comptime: Comptime,
    pub scope: ScopeStorage<SemanticState>,
}

impl Deref for SemanticSolver {
    type Target = ScopeStorage<SemanticState>;
    fn deref(&self) -> &Self::Target {
        &self.scope
    }
}

impl DerefMut for SemanticSolver {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.scope
    }
}

impl SemanticSolver {
    pub fn new(comptime: Comptime) -> Self {
        Self {
            comptime,
            scope: ScopeStorage::default(),
        }
    }
    pub fn get_mut(&mut self, name: &str) -> Result<&mut SemanticState, SemanticError> {
        self.scope
            .scopes
            .iter_mut()
            .rev()
            .find_map(|x| x.get_mut(name))
            .map(|x| &mut x.1)
            .ok_or(SemanticError::MissingVar(name.to_string()))
    }

    pub fn get(&mut self, name: &str) -> Result<(bool, CompType), SemanticError> {
        let (mutable, res) = self
            .scope
            .scopes
            .iter()
            .rev()
            .find_map(|x| x.get(name).cloned())
            .map(|x| {
                (
                    x.0,
                    match x.1 {
                        SemanticState::Solved(s) => Ok(s),
                        SemanticState::Unsolved(e) => self.type_of(&e),
                    },
                )
            })
            .ok_or(SemanticError::MissingVar(name.to_string()))?;

        Ok((mutable, res?))
    }

    /// Will return the type of the expression, while running semantic analysis on it.
    pub fn type_of(&mut self, expr: &Expr) -> Result<CompType, SemanticError> {
        Ok(match expr {
            Expr::Null => CompType::Null,
            Expr::Then { next, .. } => self.type_of(next)?,
            Expr::Block { body } => self.type_of(body)?,
            Expr::If { cond, body, else_ } => {
                self.push_scope();
                let cond_type = self.type_of(cond)?;
                if cond_type != CompType::Integer(IntegerType::Bits(8)) {
                    return Err(SemanticError::ExpectedType(
                        CompType::Integer(IntegerType::Bits(8)),
                        cond_type,
                    ));
                }
                self.pop_scope();
                self.push_scope();
                let res = if else_.is_some() {
                    self.type_of(body)?
                } else {
                    CompType::Null
                };
                self.pop_scope();
                res
            }
            Expr::Call { func, args } => {
                let mut call_args = vec![];
                for arg in args {
                    call_args.push(self.type_of(arg)?);
                }
                let func_type = self.type_of(func)?;
                let CompType::Function(args, ret) = func_type else {
                    return Err(SemanticError::ExpectedType(
                        CompType::Function(call_args, Box::new(CompType::Unknown)),
                        func_type,
                    ));
                };

                if call_args != args {
                    return Err(SemanticError::ExpectedType(
                        CompType::Function(args, ret),
                        CompType::Function(call_args, Box::new(CompType::Unknown)),
                    ));
                }

                *ret
            }
            Expr::Let {
                mutable,
                name,
                body,
            } => {
                self.push_scope();
                let body_type = self.type_of(body)?;
                self.pop_scope();
                self.register(name.clone(), *mutable, SemanticState::Solved(body_type));
                CompType::Null
            }
            Expr::Local(l) => self.get(l)?.1,
            Expr::Set { name, body } => {
                let (mutable, var_type) = self.get(name)?;
                if !mutable {
                    return Err(SemanticError::MutateImmutable(name.clone()));
                }

                self.push_scope();
                let body_type = self.type_of(body)?;
                self.pop_scope();
                if var_type != body_type {
                    return Err(SemanticError::ExpectedType(var_type, body_type));
                }

                CompType::Null
            }
            Expr::Func { args, body } => {
                self.push_scope();
                let mut typed_args = vec![];
                for (name, type_expr) in args.iter() {
                    let type_ = self
                        .comptime
                        .exec_comptime(type_expr.clone(), true)?
                        .require_type()?;

                    self.register(name.clone(), false, SemanticState::Solved(type_.clone()));
                    typed_args.push(type_);
                }
                let body = self.type_of(body)?;

                self.pop_scope();

                CompType::Function(typed_args, Box::new(body))
            }
            Expr::Literal(l) => match l {
                Literal::Num(num) => CompType::Integer(IntegerType::MinBits(min_bits(*num))),
                Literal::Bool(_) => CompType::Integer(IntegerType::Bits(8)),
            },
            Expr::Binary { lhs, op, rhs } => {
                let lhs_type = self.type_of(lhs)?;
                let rhs_type = self.type_of(rhs)?;

                if lhs_type != rhs_type {
                    return Err(SemanticError::ExpectedType(lhs_type, rhs_type));
                }

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => lhs_type,
                    BinaryOp::Eq
                    | BinaryOp::Ne
                    | BinaryOp::Lt
                    | BinaryOp::Lte
                    | BinaryOp::Gt
                    | BinaryOp::Gte => CompType::Integer(IntegerType::Bits(8)),
                }
            }
        })
    }
}
