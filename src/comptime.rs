use thiserror::Error;

use crate::{
    ast::{expr::Expr, literal::Literal},
    semantic::ScopeStorage,
    types::{CompType, IntegerType},
};

#[derive(Debug, Error)]
pub enum ComptimeError {
    #[error("Var `{0}` not in scope")]
    MissingVar(String),

    #[error("Attempting to mutate variable `{0}`, which is immutable")]
    MutateImmutable(String),

    #[error("expected a function")]
    NotAFunc,

    #[error("Expected `{0}`, but got `{1}`")]
    ArgCount(usize, usize),

    #[error("{0}")]
    Custom(String),
}

pub type ComptimeScope = ScopeStorage<ComptimeValue>;

impl ComptimeScope {
    pub fn get(&self, name: &str) -> Result<(bool, ComptimeValue), ComptimeError> {
        self.scopes
            .iter()
            .rev()
            .find_map(|x| x.get(name).cloned())
            .ok_or(ComptimeError::MissingVar(name.to_string()))
    }

    pub fn set(&mut self, name: &str, value: ComptimeValue) -> Result<(), ComptimeError> {
        let (mutable, mut_val) = self
            .scopes
            .iter_mut()
            .rev()
            .find_map(|x| x.get_mut(name))
            .ok_or(ComptimeError::MissingVar(name.to_string()))?;

        if !*mutable {
            return Err(ComptimeError::MutateImmutable(name.to_string()));
        }

        *mut_val = value;
        Ok(())
    }
}

#[derive(Clone)]
pub enum ComptimeValue {
    Literal(Literal),
    Type(CompType),
    CompFn(
        &'static dyn Fn(&mut Comptime, Vec<ComptimeValue>) -> Result<ComptimeValue, ComptimeError>,
    ),
    Func(Vec<(String, CompType)>, Expr),
    None,
}

impl ComptimeValue {
    pub fn require_type(self) -> Result<CompType, ComptimeError> {
        match self {
            Self::Type(t) => Ok(t),
            _ => Err(ComptimeError::Custom("Expected `type`".to_string())),
        }
    }
}

pub fn int_type(
    _c: &mut Comptime,
    args: Vec<ComptimeValue>,
) -> Result<ComptimeValue, ComptimeError> {
    if args.len() != 1 {
        return Err(ComptimeError::ArgCount(1, args.len()));
    }

    let ComptimeValue::Literal(Literal::Num(b)) = args[0] else {
        return Err(ComptimeError::Custom("Expected Number Literal".to_string()));
    };

    let bits: u16 = b
        .try_into()
        .map_err(|_| ComptimeError::Custom("Expected number within u16 range".to_string()))?;

    if ![8, 16, 32, 64, 128].contains(&bits) {
        return Err(ComptimeError::Custom(format!(
            "Expected one of `8`, `16`, `32`, `64`, or `128`, got {}",
            bits
        )));
    }

    Ok(ComptimeValue::Type(CompType::Integer(IntegerType::Bits(
        bits,
    ))))
}

pub struct Comptime {
    pub scope: ComptimeScope,
}

impl Default for Comptime {
    fn default() -> Self {
        let mut res = Self {
            scope: ScopeStorage::default(),
        };

        res.scope
            .register("int".to_string(), false, ComptimeValue::CompFn(&int_type));

        res
    }
}

impl Comptime {
    pub fn exec_comptime(
        &mut self,
        expr: Expr,
        scoped: bool,
    ) -> Result<ComptimeValue, ComptimeError> {
        if scoped {
            self.scope.push_scope();
        }
        let res = Ok(match expr {
            Expr::Literal(l) => ComptimeValue::Literal(l),
            Expr::Local(l) => self.scope.get(&l)?.1,
            Expr::Let {
                name,
                body,
                mutable,
            } => {
                let body_val = self.exec_comptime(*body, false)?;
                self.scope.register(name, mutable, body_val);
                ComptimeValue::None
            }
            Expr::Call { func, args } => {
                let mut solved_args = vec![];
                for arg in args {
                    solved_args.push(self.exec_comptime(arg, false)?);
                }
                let func = self.exec_comptime(*func, false)?;
                match func {
                    ComptimeValue::CompFn(inner) => inner(self, solved_args)?,
                    _ => return Err(ComptimeError::NotAFunc),
                }
            }
            Expr::Block { body } => self.exec_comptime(*body, true)?,
            _ => todo!("Expr {expr} isn't implemented in comptime yet"),
        });
        if scoped {
            self.scope.pop_scope();
        }
        res
    }
}
