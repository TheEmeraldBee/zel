use crate::{
    Error, Span,
    types::{Primitive, Type},
};

pub fn require_same(span: Span, type_a: &Type, type_b: &Type) -> Result<(), Error> {
    match type_a == type_b {
        true => Ok(()),
        false => Err(Error::new(
            span,
            format!(
                "Types for this expression need to be the same, expected `{}`, got `{}`",
                type_a, type_b
            ),
        )),
    }
}
pub fn require_true(span: Span, val: bool, fail_msg: impl ToString) -> Result<(), Error> {
    match val {
        true => Ok(()),
        false => Err(Error::new(span, fail_msg)),
    }
}

pub fn require_fn(span: Span, fn_type: &Type, args: &Vec<Type>) -> Result<Type, Error> {
    match fn_type {
        Type::Primitive(Primitive::RustFunc(expected_ret)) => Ok(*expected_ret.clone()),
        Type::Primitive(Primitive::Func(arg_types, expected_ret)) => {
            for (wanted, got) in args.iter().zip(arg_types) {
                require_same(span, wanted, got)?;
            }
            Ok(*expected_ret.clone())
        }
        _ => Err(Error::new(
            span,
            format!(
                "Callable can only be `rustfn` or `function`, found `{}`",
                fn_type
            ),
        )),
    }
}
