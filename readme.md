# Hatch Compiler - Bird Language Compiler

A compiler implementation for the Bird programming language, written in OCaml using the Dune build system. This project compiles Bird language source files (`.bird` extension) into executable binaries.

## Features

- **Arithmetic Operations**: Support for basic arithmetic expressions with proper operator precedence
- **Variable Bindings**: `let...in` expressions for local variable binding  
- **First-Class Functions**: Functions are values that can be passed, returned, and stored (OCaml-like semantics)
- **Control Flow**: Conditional expressions with `if` statements
- **Type Checking**: Built-in type predicates (`isbool`, `isint`) and comparison operators
- **Error Handling**: Comprehensive error reporting and runtime error handling
- **Garbage Collection**: Integrated garbage collector for memory management

## Project Structure

```
src/
├── compiler/        # Core compiler implementation
│   ├── assembly.ml  # Assembly code generation
│   ├── builder.ml   # Build orchestration
│   ├── compiler.ml  # Main compilation logic
│   └── ...
├── language/        # Language parsing and AST definitions
│   ├── asts.ml      # Abstract syntax tree definitions
│   ├── lexer.mll    # Lexical analyzer
│   ├── parser.mly   # Parser grammar
│   └── ...
├── main/            # Entry point
│   └── hatch.ml     # Main executable
└── tests/           # Test suite
    └── tests.ml

test_code/           # Test programs organized by feature
├── arithmetic.bird  # Basic arithmetic
├── let1.bird       # Variable binding
├── bluebird/       # Control flow tests
├── cardinal/       # Error handling tests  
├── dove/           # Function definition tests
└── ...
```

## Building and Running

### Prerequisites
- OCaml (with Dune build system)
- Make (optional, for convenience)

### Build
```bash
make build
# OR
dune build
```

### Run Compiler
```bash
./hatch <filename.bird>
```

### Run Tests
```bash
make test
# OR
./tests
```

## Bird Language Syntax

### Basic Arithmetic
```bird
3 * (1 + (4 - 2) - (3 * 2))
```

### Variable Binding
```bird
let x = 3 in 4 + x
```

### Function Definition
```bird
def f x = 
  x + 1
end

f 5
```

### Conditional Expressions
```bird
if x > 0 then x else 0
```

## Test Organization

The test suite is organized by language features:
- **Basic**: Arithmetic and basic expressions
- **Bluebird**: Control flow and conditionals
- **Cardinal**: Error handling and edge cases
- **Dove**: Function definitions and calls
- **Eagle/Falcon/Gull**: Advanced language features

## Development

The compiler is structured as a traditional multi-pass compiler:
1. **Lexing**: Tokenization of source code
2. **Parsing**: Construction of abstract syntax tree
3. **Well-formedness**: Static analysis and type checking
4. **Compilation**: Code generation to assembly
5. **Assembly**: Final executable generation

The project uses OCaml's Menhir parser generator and includes comprehensive error handling throughout the compilation pipeline.

## Credits

This compiler project was developed as part of **CS75: Compilers** at Swarthmore College, taught by **Professor Zachary Palmer**. The project follows a progressive series of language implementations, building from basic arithmetic expressions to a full functional programming language with garbage collection.