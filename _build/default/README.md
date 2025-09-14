# OCaml SIMD Test with OxCaml

This project demonstrates SIMD (Single Instruction, Multiple Data) operations using OCaml with the OxCaml compiler.

## Features

- Custom option type implementation
- Array addition with imperative style
- SIMD-optimized array operations using Float64x2 and Float32x4
- SIMD string operations with Int8x16
- Immutable array support

## Build Instructions

### Prerequisites
- OxCaml compiler (OCaml 5.2+ox variant)
- Dune build system
- `stdlib_upstream_compatible` package

### Building

```bash
# Build the project
dune build

# Run the executable
dune exec ./main.exe

# Or build and run in one command
dune exec oxcaml-simd-test
```

### Alternative compilation (without dune)
```bash
ocamlfind ocamlopt -O3 -package stdlib_upstream_compatible -linkpkg main.ml -o main
./main
```

## Code Structure

- `main.ml` - Main source file containing all SIMD operations
- `dune` - Dune build configuration
- `dune-project` - Project metadata

## SIMD Operations

The code demonstrates:
1. **Float64x2**: 2-element double precision vector operations
2. **Float32x4**: 4-element single precision vector operations  
3. **Int8x16**: 16-element byte vector for string operations

Each SIMD function processes arrays in chunks using vector instructions for better performance.