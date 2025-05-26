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

pub mod parser;

pub mod compiler;
