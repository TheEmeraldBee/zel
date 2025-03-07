# Zel: A language with the goal of making writing code more fun!
This language is my personal "perfect" language, but I would love to hear your feedback.

## Progress
This project is currently in **HEAVY** development, meaning that it is being worked on, and is in an unusable state
There are constant changes, and nothing is currently compiled, meaning the entire language is interpreted.

## Milestones
- [x] Parsing Of Basic Language Features
- [x] Basic Evaluation
- [x] Basic Scope
- [x] Simple Implementation for Main function, and code in root of file being seperate (const variables **ONLY**) Using Top-Level definition checking to get this
- [x] Add for loops, which allow for functions of while, and loop in rust
- [x] Make functions self aware. (Meaning that functions are aware of their own definition, and can be called recursively without being in top level)
- [x] Add additional syntax to `for` to allow for `for var i = 0; i < 100; i = i + 1;` where you have an init, condition, and after grouping
- [x] Add Increment `++` and decrement `--` math shortcuts
- [x] Add `+=`, `-=`, `*=`, `/=`
- [x] Code Validation before interpretation (Make sure that variables exist for now before code is executed)
  - This was a big one, now variables are checked for both mutability, and existing for every expression before interpretation, making it a compile time issue instead
  of a runtime issue!
- [x] Type Checking (Basic typing, this is a big one baby :))
- [ ] Convert Interpreter to `cranelift` + `faerie` compiler!
- [ ] Add control flow keywords `break`, `continue`, and `return`, where `return` can return a value, but the other two can not.
- [ ] **MUCH** More to come (I will always try to have 3 steps more than what is done)

## Examples
```
// In order to write a recursive function, the first parameter into a function should be self. This will be converted into the function you are working with
const fib = fn(n: num) -> num {
	if n <= 1 {
		n
	} else {
		this(n - 1) + this(n - 2)
	}
};

const main = fn() {
  print("Input a number:");
  const input = read_num();
  print(fib(input));
};
```
More examples can be found [here](https://github.com/TheEmeraldBee/zel/tree/master/examples)

## Language Definition

### Concepts
- importing is as simple as using `use filename.zl`
- Errors as values
- nullables are known
- Statically typed
- Pass by pointer, but pass ownership if not using copy (`%`) or a pointer (`*`)
- Ownership but no borrowing (memory handling is manual, but allocations can be released automatically)
- `.` before a value interprets the type based on context (`.enummember`)
- `var` keyword for creating mutable variables
- `const` keyword for creating immutable variables
- Generics are handled at compile time, define function templates using (`temp`)
- Runtime polymorphism is as easy as adding `dyn` to a template type

### Primitive Types:
- ints (`i8`, `i16`, `i32`, `i64`, `i128`)
  - same syntax for types as rust (`15i8` for example)
- unsigned ints (`u8`, `u16`, `u32`, `u64`, `u128`)
- floats (`f16`, `f32`, `f64`, `f128`)
- bools (`bool`: `true`, `false`)
- strings (`string`: `"abacadaba"`)
- size (unsigned int the size of the architecture)
  - same syntax for types as rust (`120size`)
- lists (Vector types that can be defined as either sized or unsized)
  - `[typename]` would mark a type as a list of that type
  - `[typename; size]` a list with the size of the value in the size position
  - `[value, ...]` would make a list with the given elements.
  - `[value, ...; size]` would make a list with the given elements.
- dicts (Hash map types that may have any hashable type as a key, and any type as a value)
  - `{typename, typename}` would create a dictionary with the given types
  - `{key => value, ...}` would create a dictionary with the given keys and values already initialized
- option (a marked type that could be null)
  - `?typename` would mark the type of a variable as optional
- error (a marked type that could be an error)
  - `!typename` woiuld mark that the type could be an error
- structs (product types)
  - `struct { fieldname: typename, ... }`
- enums (sum types)
  - `enum { membername: type, ... }`
- functions
  - `const a = fn(param: paramtype, ...)` would be a function with inputs but no outputs
  - `const b = fn(param: paramtype, ...) -> typename` would be a function with inputs and an output
  - `const c = fn() {}` would be a function that is defined and is a value not a type
- templates
  - ```
    const MyTemplate = temp {
      var x: i32; // A variable that must be on the type implementing the template
      mut var y: i64 = 5; // A mutable variable that is on every type implementing the template, and has a default value.

      const a: fn() -> typename; // A function that would have to be implemented
      const b = fn() -> typename {}; // A function with a default implementation
      const c: fn(*self) -> typename; // A function that passes self
      const d: fn(*mut self) -> typename; // A function that passes a mutable self
    };
    ```
  - Taking these templates into functions is pretty easy too!
    This would allow for this function to take anything using MyTemplate, and work on it!
    ```
    const handle = fn(x: *dyn MyTemplate) {
      x.a();
      x.y = 5;
    };
    ```
- polymorphism
  - `dyn` keyword allows for the type to be handled at runtime
    - `dyn var x: MyTemplate = MyType{a: 5}` would mean that this variable could then be changed to anything that implements the MyTemplate type
