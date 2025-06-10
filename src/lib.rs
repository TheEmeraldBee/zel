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

/// Module contains code that translates tokens of a file into a single Expression,
pub mod parser;

/// Module contains code for both comptime and the compiler to handle scope.
pub mod scope;

/// Module contains code for definining types, as well as primitive types.
pub mod types;

/// Module contains code for both comptime and the compiler to handle values.
pub mod value;

/// Module contains code for handling many instances of function types.
pub mod func;

/// Module contains code that to execute `zel` code during compile time
pub mod comptime;

/// Module contains code for compiling zel code to LLVM IR
pub mod compiler;
