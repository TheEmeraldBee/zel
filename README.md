# Jos: A language with the goal of making writing code more fun!
This language is my personal "perfect" language, but I would love to hear your feedback.

## Example
```
var myvar = 12i8; // Create a basic variable
myvar = 5; // Works! the `var` keyword means that variables are mutable!

const myvar = 12i8; // Create a basic variable with const
myvar = 5; // Doesn't work! the `const` keyword means that variables are immutable!

// Type Aliasing!
const MyType = u32;

// Functions!
const my_func = fn(name: string) {
  print("Hello, {}!\n", name);
};

// Create a template
const MyTemplate = temp {
  const say_hi: fn(*self);
};

mut var MyMutableFunction = fn(title: string) {
  print("the book's name is {}", title);
}

// Create a struct
const MyType = struct(MyTemplate) {
  name: string,

  const say_hi = fn(*self) {
    print("Hello!");
  }
};

const my_template_func = fn(user: *MyTemplate) {
  user.say_hi()
}

// Closures
const hi = fn() {print("Hello, world!")};
hi();

fn() {print("hello, world!")}();

// Use file
const my_file = use hello_world;
const my_file = use hello_world_me.zl;

```

## Language Definition

### Concepts
- importing is as simple as using `use filename.zl`
- Errors as values
- nullables are known
- Statically typed
- Pass by pointer, but pass ownership if not using copy (`%`) or a pointer (`*`)
- Ownership but no borrowing (memory handling is manual, but allocations can be released automatically)
- `.` before a value interprets the type based on context (`.enummember`)
- `var` keyword for creating variables
- `const` keyword for variables that are known at compiletime
- Immutable by default (`mut` to make var mutable)
- Generics are handled at compile time define function templates using (`temp`)
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
    const handle = fn(x: *mut MyTemplate) {
      x.a();
      x.y = 5;
    };
    ```
- polymorphism
  - `dyn` keyword allows for the type to be handled at runtime
    - `dyn var x: MyTemplate = MyType{a: 5}` would mean that this variable could then be changed to anything that implements the MyTemplate type
