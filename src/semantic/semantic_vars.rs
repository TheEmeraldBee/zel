use crate::{Error, Spanned, parse::Expr, types::Type};

use super::{SemanticContext, analyze_expr};

#[derive(Debug, Clone)]
pub enum TopLevelSemantic<'src> {
    Solved(SemanticVar),
    Unsolved(Spanned<Expr<'src>>),
}

impl<'src> TopLevelSemantic<'src> {
    pub fn solved(self, ctx: &mut SemanticContext<'src>) -> Result<SemanticVar, Error> {
        Ok(match self {
            Self::Solved(v) => v,
            Self::Unsolved(expr) => SemanticVar::immutable(analyze_expr(&expr, ctx)?),
        })
    }
}

#[derive(Debug, Clone)]
pub struct SemanticVar {
    pub type_: Type,
    pub mutable: bool,
}

impl SemanticVar {
    pub fn new(mutable: bool, type_: Type) -> Self {
        Self { type_, mutable }
    }
    pub fn mutable(type_: Type) -> Self {
        Self {
            type_,
            mutable: true,
        }
    }

    pub fn immutable(type_: Type) -> Self {
        Self {
            type_,
            mutable: false,
        }
    }
}
