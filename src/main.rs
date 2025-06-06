use std::{fs, io::stdin, path::PathBuf};

use anyhow::anyhow;
use clap::Parser;
use zel::{
    ast::{expr::Expr, literal::Literal, top_level::TopLevel},
    comptime::{Comptime, ComptimeError},
    func::FunctionScope,
    lexer::Lexer,
    scope::{Scope, Solver, Variable, VariableValue},
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
    _scope: &mut Scope<Value>,
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

fn str_input(
    comptime: &mut Comptime,
    scope: &mut Scope<Value>,
    args: Vec<Value>,
) -> Result<Value, ComptimeError> {
    debug(comptime, scope, args)?;

    let mut line = "".to_string();
    stdin().read_line(&mut line).unwrap();

    Ok(Value::Literal(Literal::String(line.trim().to_string())))
}

fn cast_int(
    _comptime: &mut Comptime,
    _scope: &mut Scope<Value>,
    args: Vec<Value>,
) -> Result<Value, ComptimeError> {
    if args.len() != 1 {
        return Err(ComptimeError::Expected("1 argument".to_string()));
    }

    Ok(match args[0].clone() {
        Value::Literal(l) => match l {
            Literal::Num(n) => Value::Literal(Literal::Num(n)),
            Literal::Bool(b) => Value::Literal(Literal::Num(if b { 1 } else { 0 })),
            Literal::String(s) => match s.parse::<i64>() {
                Ok(n) => Value::Literal(Literal::Num(n)),
                Err(e) => return Err(ComptimeError::Expected(e.to_string())),
            },
        },
        _ => return Err(ComptimeError::Expected("literal".to_string())),
    })
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
    comptime.register_func("input", &str_input);
    comptime.register_func("to_int", &cast_int);
    comptime.register_func("int", &|_, _, v| {
        if v.len() != 1 {
            return Err(ComptimeError::Expected("1 argument".to_string()));
        }

        let Value::Literal(Literal::Num(bits)) = v[0] else {
            return Err(ComptimeError::Expected("number literal".to_string()));
        };

        Ok(Value::Type(Type::Integer(bits as u8)))
    });
    comptime.register_func("array", &|_, _, v| {
        if v.len() != 2 {
            return Err(ComptimeError::Expected("2 arguments".to_string()));
        }

        let Value::Type(t) = v[0].clone() else {
            return Err(ComptimeError::Expected("type".to_string()));
        };

        let Value::Literal(Literal::Num(n)) = v[1] else {
            return Err(ComptimeError::Expected("number literal".to_string()));
        };

        Ok(Value::Type(Type::Array(n as usize, Box::new(t))))
    });
    comptime.register_func("slice", &|_, _, v| {
        if v.len() != 1 {
            return Err(ComptimeError::Expected("1 argument".to_string()));
        }

        let Value::Type(t) = v[0].clone() else {
            return Err(ComptimeError::Expected("type".to_string()));
        };

        Ok(Value::Type(Type::Slice(Box::new(t))))
    });
    comptime.register_func("len", &|_, _, v| {
        if v.len() != 1 {
            return Err(ComptimeError::Expected("1 argument".to_string()));
        }

        match &v[0] {
            Value::Array { type_: _, elements } => {
                Ok(Value::Literal(Literal::Num(elements.len() as i64)))
            }
            _ => return Err(ComptimeError::Expected("slice | array".to_string())),
        }
    });
    let mut scope = Scope::default();
    scope.register(
        "string",
        Variable {
            mutable: false,
            value: VariableValue::Initialized(Value::Type(Type::String)),
        },
    );
    scope.register(
        "bool",
        Variable {
            mutable: false,
            value: VariableValue::Initialized(Value::Type(Type::Bool)),
        },
    );
    scope.register(
        "func",
        Variable {
            mutable: false,
            value: VariableValue::Initialized(Value::Type(Type::Func)),
        },
    );
    scope.register(
        "type",
        Variable {
            mutable: false,
            value: VariableValue::Initialized(Value::Type(Type::Type)),
        },
    );
    scope.register_top_level(top_level);

    let mut funcs = FunctionScope::default();

    let res = comptime.solve(
        &mut scope,
        &mut funcs,
        &Expr::Call {
            func: Box::new(Expr::Local("main".to_string())),
            args: vec![],
        },
    )?;

    if let Value::Literal(Literal::Num(a)) = res {
        if a != 0 {
            std::process::exit(a as i32);
        }
    }

    Ok(())
}
