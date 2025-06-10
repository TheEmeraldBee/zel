// src/main.rs

use std::{fs, io::stdin, path::PathBuf, process::Command};

use anyhow::anyhow;
use clap::Parser;
use inkwell::{
    OptimizationLevel,
    context::Context,
    passes::PassBuilderOptions,
    targets::{InitializationConfig, Target, TargetMachine},
};
use zel::{
    ast::{expr::Expr, literal::Literal, top_level::TopLevel},
    compiler::Compiler,
    comptime::{Comptime, ComptimeError},
    func::FunctionScope,
    lexer::Lexer,
    scope::{Scope, Variable, VariableValue},
    types::Type,
    value::Value,
};

#[derive(clap::Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
struct Args {
    file: PathBuf,

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

fn setup_comptime_scope(
    top_level: TopLevel,
    initial_path: PathBuf,
) -> (Comptime, Scope<Value>, FunctionScope) {
    let mut comptime = Comptime::new(initial_path);
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
    let mut scope = Scope::default();
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
    scope.register_top_level(&top_level);

    let mut funcs = FunctionScope::default();

    for (name, expr) in top_level.iter() {
        if let Expr::Func {
            args,
            body,
            return_type,
        } = expr
        {
            let id = funcs.register(args.clone(), *body.clone(), *return_type.clone());
            scope
                .force_set(name, VariableValue::Initialized(Value::Function(id)))
                .unwrap();
        }
    }

    (comptime, scope, funcs)
}

fn compile(
    args: Args,
    _top_level: TopLevel,
    mut comptime: Comptime,
    mut scope: Scope<Value>,
    mut funcs: FunctionScope,
) -> anyhow::Result<()> {
    let context = Context::create();
    let module_name = args.file.file_stem().unwrap().to_str().unwrap();
    let mut compiler = Compiler::new(&context, module_name, &mut comptime, &mut scope, &mut funcs);

    compiler.monomorphize_and_compile_fn("main", &[])?;

    Target::initialize_native(&InitializationConfig::default()).map_err(|x| anyhow!("{}", x))?;
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::Default,
            inkwell::targets::RelocMode::PIC,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();

    let passes: &[&str] = &[
        "instcombine",
        "reassociate",
        "gvn",
        "simplifycfg",
        "mem2reg",
    ];

    compiler
        .module
        .run_passes(
            passes.join(",").as_str(),
            &target_machine,
            PassBuilderOptions::create(),
        )
        .unwrap();

    let obj_path = if args.out.extension().map_or(false, |e| e == "o") {
        args.out.clone()
    } else {
        args.out.with_extension("o")
    };
    target_machine
        .write_to_file(
            &compiler.module,
            inkwell::targets::FileType::Object,
            &obj_path,
        )
        .unwrap();

    if args.out.extension().map_or(true, |e| e != "o") {
        let status = Command::new("cc")
            .arg(&obj_path)
            .arg("-o")
            .arg(&args.out)
            .status()?;

        if !status.success() {
            anyhow::bail!("Linking failed");
        }

        fs::remove_file(obj_path)?;
    }

    Ok(())
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let src = fs::read_to_string(args.file.clone())?;

    let tokens = Lexer::lex(&src)?;

    let ast = match zel::parser::Parser::parse(tokens) {
        Ok(t) => t,
        Err(e) => return Err(anyhow!("{}", e.to_string())),
    };

    let mut top_level = TopLevel::default();
    top_level.populate(ast)?;
    top_level.require_fn("main", vec![])?;

    let initial_dir = args
        .file
        .parent()
        .ok_or_else(|| anyhow!("Could not get parent directory of input file"))?
        .to_path_buf();

    let (comptime, scope, funcs) = setup_comptime_scope(top_level.clone(), initial_dir);

    compile(args, top_level.clone(), comptime, scope, funcs)?;

    Ok(())
}
