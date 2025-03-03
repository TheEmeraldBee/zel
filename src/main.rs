use chumsky::prelude::*;
use error::Error;
use error::print_errors;
use eval::Context;
use eval::Variable;
use semantic::SemanticContext;
use semantic::SemanticVar;
use semantic::TopLevel;
use semantic::analyze_top_level;
use types::Primitive;
use types::Type;
use value::Value;

use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::rc::Rc;

pub mod error;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub mod lexer;

pub mod value;

pub mod parse;

pub mod semantic;

pub mod types;

pub mod eval;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");

    let (tokens, mut errs) = lexer::lexer().parse(src.as_str()).into_output_errors();

    let mut ctx = Context::default();
    ctx.insert_global(
        "print",
        Variable::immutable(Value::Rust(Rc::new(Box::new(|_, args| {
            for arg in &args {
                print!("{}", arg);
            }
            println!();
            Ok(Value::Null)
        })))),
    );

    ctx.insert_global(
        "read_line",
        Variable::immutable(Value::Rust(Rc::new(Box::new(|span, args| {
            if !args.is_empty() {
                return Err(Error::new(span, "read_line should not have any arguments!"));
            }

            let mut s = String::new();

            std::io::stdin().read_line(&mut s).unwrap();

            s = s.trim().to_string();

            Ok(Value::Str(s))
        })))),
    );

    ctx.insert_global(
        "read_num",
        Variable::immutable(Value::Rust(Rc::new(Box::new(|span, args| {
            if !args.is_empty() {
                return Err(Error::new(span, "read_line should not have any arguments!"));
            }

            let mut s = String::new();

            std::io::stdin().read_line(&mut s).unwrap();

            let num = s.trim().parse::<f64>().unwrap();

            Ok(Value::Num(num))
        })))),
    );

    let globals = BTreeMap::from([
        (
            "read_num",
            SemanticVar::immutable(Type::Primitive(Primitive::RustFunc(Box::new(
                Type::Primitive(Primitive::Num),
            )))),
        ),
        (
            "read_line",
            SemanticVar::immutable(Type::Primitive(Primitive::RustFunc(Box::new(
                Type::Primitive(Primitive::String),
            )))),
        ),
        (
            "print",
            SemanticVar::immutable(Type::Primitive(Primitive::RustFunc(Box::new(
                Type::Primitive(Primitive::Null),
            )))),
        ),
    ]);

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parse::parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(
                tokens
                    .as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            )
            .into_output_errors();

        if let Some((ast, _file_span)) = ast.filter(|_| errs.is_empty() && parse_errs.is_empty()) {
            let mut top_level = TopLevel::default();

            // Analyze top level and find elements
            match analyze_top_level(&ast, &mut top_level) {
                Ok(_) => (),
                Err(e) => errs.push(Rich::custom(e.span, e.msg)),
            }

            // Do type checking, and variable solving
            match SemanticContext::solve(&top_level, globals) {
                Ok(_) => {}
                Err(e) => errs.push(Rich::custom(e.span, e.msg)),
            };

            // Ensure there were no errors solving for top-level
            if errs.is_empty() {
                // Insert top level declarations into context
                ctx.insert_top_level(top_level);

                // Solve the main function by calling it!
                let main_expr = (
                    parse::Expr::Call(
                        Box::new((parse::Expr::Local("main"), ast.1)),
                        (Vec::new(), ast.1),
                    ),
                    ast.1,
                );

                // Eval the main function, printing if success, and building errors
                match eval::eval(&main_expr, &mut ctx) {
                    Ok(_) => println!("\n\nCode successfully exited"),
                    Err(e) => errs.push(Rich::custom(e.span, e.msg)),
                }
            }
        }

        parse_errs
    } else {
        Vec::new()
    };

    print_errors(&filename, &src, errs, parse_errs);
}
