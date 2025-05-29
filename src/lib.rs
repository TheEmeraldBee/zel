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

/// Handles all types in the language
/// this includes structs, primitives,
/// arrays, etc...
pub mod types;

/// Module contains code that translates tokens of a file into a single Expression,
pub mod parser;

/// A basic zel interpreter, that also handles functions that create types.
pub mod comptime;

/// Module contains code to handle type checking, and variable existence before it becomes a IR error
pub mod semantic;

/// Module contains systems used to translate Expressions into Cranelift IR and object code
pub mod compiler;
