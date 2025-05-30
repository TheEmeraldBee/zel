use std::{fs, path::PathBuf};

use anyhow::anyhow;
use clap::Parser;
use zel::{
    ast::{expr::Expr, literal::Literal, top_level::TopLevel},
    comptime::{Comptime, ComptimeError},
    lexer::Lexer,
    scope::{Scope, Variable},
    types::Type,
    value::Value,
};

#[derive(clap::Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
struct Args {
    /// The file to compile
    file: PathBuf,

    /// The output name of the file. If the file path's type is `.o`, will output object file, otherwise, it will be the binary fully linked
    out: PathBuf,
}

fn debug(
    _comptime: &mut Comptime,
    _scope: &mut Scope,
    args: Vec<Value>,
) -> Result<Value, ComptimeError> {
    println!(
        "{}",
        args.iter()
            .map(|x| x.to_string())
            .reduce(|lhs, rhs| format!("{lhs} {rhs}"))
            .unwrap_or_default()
    );
    Ok(Value::Null)
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let src = fs::read_to_string(args.file)?;

    let tokens = Lexer::lex(&src)?;

    let ast = match zel::parser::Parser::parse(tokens) {
        Ok(t) => t,
        Err(e) => return Err(anyhow!("{}", e.to_string())),
    };

    let mut top_level = TopLevel::default();

    // Populate the top-level declarations with the generated ast
    top_level.populate(ast)?;

    // Ensure, after populating the ast with functions, that the function `main` exists
    top_level.require_fn("main", vec![])?;

    let mut comptime = Comptime::default();
    comptime.register_func("debug", &debug);
    comptime.register_func("int", &|_, _, v| {
        if v.len() != 1 {
            return Err(ComptimeError::Expected("1 argument".to_string()));
        }

        let Value::Literal(Literal::Num(bits)) = v[0] else {
            return Err(ComptimeError::Expected("number literal".to_string()));
        };

        Ok(Value::Type(Type::Integer(bits as u8)))
    });
    let mut scope = Scope::default();
    scope.register(
        "string",
        Variable {
            mutable: false,
            value: Value::Type(Type::String),
        },
    );
    scope.register(
        "bool",
        Variable {
            mutable: false,
            value: Value::Type(Type::Bool),
        },
    );
    scope.register(
        "type",
        Variable {
            mutable: false,
            value: Value::Type(Type::Type),
        },
    );
    scope.register_top_level(top_level);

    let res = comptime.execute(
        &mut scope,
        &Expr::Call {
            func: Box::new(Expr::Local("main".to_string())),
            args: vec![],
        },
        false,
    )?;

    if let Value::Literal(Literal::Num(a)) = res {
        if a != 0 {
            std::process::exit(a as i32);
        }
    }

    Ok(())
}
