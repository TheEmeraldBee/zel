# Zel
Zel is a compiled, simplistic, systems level programming language.
It uses concepts similarly to zig, but has a more programmer friendly syntax, and is focused on maintaining good ux.

# Zig like?
Uses comptime instead of ugly generic markers to write code

# Code Examples
The below example is a basic implementation of a resizable vector.
```rust
let i64 = int(64)
let i8 = int(8)

extern fn malloc(size: i64) -> *i8
extern fn realloc(ptr: *i8, new_size: i64) -> *i8

extern fn puts (input: *i8) -> i64

let Vec = fn(item_type: type) -> type {
  struct {
    ptr: *item_type,
    len: i64,
    cap: i64
  }
}

let vec_new = fn(item_type: type) -> Vec(item_type) {
  Vec(item_type) [
    ptr: malloc(0), // Start with a null-like pointer
    len: 0,
    cap: 0
  ]
}

let append = fn(self: Vec(i8), item: i8) -> Vec(i8) {
  let mut self = self;
  if self.len == self.cap {
    let new_cap = if self.cap == 0 { 4 } else { self.cap * 2 };
    let new_size = new_cap;

    self.ptr = realloc(self.ptr, new_size);
    self.cap = new_cap;
  };

  self.ptr[self.len] = item;
  self.len = self.len + 1;

  self
}

let main = fn() -> i64 {
  let mut my_vec = vec_new(i8);

  for let mut i: i8 = 97; i < 105; i = i + 1 {
      my_vec = append(my_vec, i);
  };

  puts(my_vec.ptr);
  0
}
```

# Project Milestones
- [x] Basic Parsing Implementation
- [x] Write basic comptime interpreter
- [x] Implement enough of a compiler to create a vec type
  - Fun Fact: Just this turned the compiler into 1200 lines of code :)

- [ ] String Literal Parsing Improvements: Allow for string literals to have escape characters
- [ ] Better Separation of Comptime and Compiled Code. Introduce `const` keywork that will run code using comptime instead of compiling it.
- [ ] `const top_level = fn() {}`, Replace most usages of let in top-level with const to show that the variable itself is comptime executed.

- [ ] Struct Functions. Allow for structures to define functions 
- [ ] Top-Level is just a struct. Make top-level declarations structures, making importing much more simple.
  - Important: Top-Level will support fields the same as structs.
  - This should also fix the weird, top-level declarations don't require `;` but blocks do.
  
- [ ] File Importing. `let my_module = import("./my_module")` will allow separation of types much easier, and will help to set up for structs with functions
- [ ] Basic std library. Now that file importing will exist, begin writing a simple set of c wrappers to support file loading and misc updates.

- [ ] Function Pointers. Allow for functions to be called in all contexts, as well as inline functions (should allow for much more expressive code).
