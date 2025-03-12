use std::{fs, path::PathBuf, process::Command};

use anyhow::anyhow;
use clap::Parser;
use zel::{ast::top_level::TopLevel, compiler::Compiler, lexer::Lexer};

#[derive(clap::Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
struct Args {
    /// The file to compile
    file: PathBuf,

    /// The output name of the file. If the file path's type is `.o`, will output object file, otherwise, it will be the binary fully linked
    out: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let mut args = Args::parse();

    let args2 = args.clone();
    let out_name = args2.out.file_name().unwrap().to_str().unwrap();

    let src = fs::read_to_string(args.file)?;

    let tokens = Lexer::lex(&src)?;

    let ast = match zel::parser::Parser::parse(&tokens) {
        Ok(t) => t,
        Err(e) => return Err(anyhow!("{}", e.to_string())),
    };

    println!("{ast:#?}");

    let mut compiler = Compiler::new(out_name)?;

    let mut top_level = TopLevel::default();

    // Populate the top-level declarations with the generated ast
    top_level.populate(ast)?;

    // Ensure, after populating the ast with functions, that the function `main` exists
    top_level.require_fn("main", vec![])?;

    // Compile each expression in the top-level
    for (name, expr) in top_level.finish() {
        compiler.compile(name, expr)?;
    }

    let path = args.out.clone();

    // Write out the code to a .o file.
    args.out.set_extension("o");
    let bin = compiler.finish();
    let file = fs::File::create(args.out.clone()).expect("File should be successfully created");
    bin.object
        .write_stream(file)
        .expect("File should be successfully written to");

    if path
        .extension()
        .map(|x| x.to_str() != Some("o"))
        .unwrap_or(true)
    {
        println!("Linking:");
        // The user wants us to link, so run gcc on the file.
        Command::new("gcc")
            .args(["-o", path.to_str().unwrap(), args.out.to_str().unwrap()])
            .status()?;

        fs::remove_file(args.out)?;
    }

    Ok(())
}
