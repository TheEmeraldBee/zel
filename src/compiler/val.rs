use crate::ast::ops::BinaryOp;

use super::{error::CompilerError, translator::Translator};
use cranelift::{module::FuncId, prelude::*};

#[derive(Copy, Clone, Debug)]
pub enum TranslationValue {
    Null,
    Value(Value),
    Func(FuncId),
}

impl TranslationValue {
    pub fn require_value(&self) -> Result<Value, CompilerError> {
        match self {
            Self::Value(v) => Ok(*v),
            _ => Err(CompilerError::InvalidType(format!(
                "Expected `value`, got {:?}",
                self
            ))),
        }
    }

    pub fn require_fn(&self) -> Result<FuncId, CompilerError> {
        match self {
            Self::Func(id) => Ok(*id),
            _ => Err(CompilerError::InvalidType(format!(
                "Expected `function`, got {:?}",
                self
            ))),
        }
    }

    pub fn bin_op(
        &self,
        rhs: &Self,
        op: BinaryOp,
        translator: &mut Translator<'_>,
    ) -> Result<Self, CompilerError> {
        let lhs = self.require_value()?;
        let rhs = rhs.require_value()?;

        Ok(val(match op {
            BinaryOp::Add => translator.builder.ins().iadd(lhs, rhs),
            BinaryOp::Sub => translator.builder.ins().isub(lhs, rhs),
            BinaryOp::Mul => translator.builder.ins().imul(lhs, rhs),
            BinaryOp::Div => translator.builder.ins().sdiv(lhs, rhs),
            BinaryOp::Eq => translator.builder.ins().icmp(IntCC::Equal, lhs, rhs),
            BinaryOp::Ne => translator.builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
            BinaryOp::Lt => translator
                .builder
                .ins()
                .icmp(IntCC::SignedLessThan, lhs, rhs),
            BinaryOp::Lte => translator
                .builder
                .ins()
                .icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
            BinaryOp::Gt => translator
                .builder
                .ins()
                .icmp(IntCC::SignedGreaterThan, lhs, rhs),
            BinaryOp::Gte => {
                translator
                    .builder
                    .ins()
                    .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
            }
        }))
    }
}

pub fn val(value: Value) -> TranslationValue {
    TranslationValue::Value(value)
}

pub fn func(id: FuncId) -> TranslationValue {
    TranslationValue::Func(id)
}
