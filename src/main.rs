use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::prelude::*;
use eval::Error;
use eval::Variable;

use std::collections::HashMap;
use std::env;
use std::fs;
use std::rc::Rc;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

pub mod lexer;

pub mod value;
use value::Value;

pub mod parse;
use parse::{parser, BinaryOp, Expr};

pub mod eval;
use eval::eval_expr;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");

    let (tokens, mut errs) = lexer::lexer().parse(src.as_str()).into_output_errors();

    let mut globals = HashMap::new();
    globals.insert(
        "print".to_string(),
        Variable::immutable(Value::Rust(Rc::new(Box::new(|_, args| {
            for arg in &args {
                print!("{}", arg);
            }
            println!();
            Ok(Value::Null)
        })))),
    );

    globals.insert(
        "read_line".to_string(),
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

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(
                tokens
                    .as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            )
            .into_output_errors();

        if let Some((ast, _file_span)) = ast.filter(|_| errs.is_empty() && parse_errs.is_empty()) {
            match eval_expr(ast, &mut globals).0 {
                Ok(_) => println!("\n\nCode successfully exited"),
                Err(e) => errs.push(Rich::custom(e.span, e.msg)),
            }
        }

        parse_errs
    } else {
        Vec::new()
    };

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| format!("{:?}", tok))),
        )
        .for_each(|e| {
            Report::build(
                ReportKind::Error,
                (filename.clone(), e.span().start..e.span().start),
            )
            .with_message(e.to_string())
            // .with_label(
            //     Label::new((filename.clone(), e.span().into_range()))
            //         .with_message(e.reason().to_string())
            //         .with_color(Color::Red),
            // )
            .with_labels(e.contexts().map(|(label, span)| {
                Label::new((filename.clone(), span.into_range()))
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            .print((filename.clone(), Source::from(src.clone())))
            .unwrap()
        });
}
