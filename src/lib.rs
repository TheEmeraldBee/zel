#[macro_export]
macro_rules! otry {
    ($fallible:expr) => {
        match $fallible {
            Ok(t) => t,
            Err(e) => return Some(Err(e.into())),
        }
    };
}

/// Contains tokens parsed from input text
pub mod token;

/// Module contains code that translates raw-text into a stream of tokens
pub mod lexer;

/// Contains types showing the AST
/// This also contains helper traits and methods
/// to work with the AST
///
/// Also contains types and typing systems
pub mod ast;

/// Module contains code that translates a token stream into an AST set
pub mod parser;

/// Contains code that translates AST code into
/// a compiled executable, returning the ModuleOutput
pub mod compiler;
