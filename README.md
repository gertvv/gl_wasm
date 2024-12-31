# gl_wasm

[![Package Version](https://img.shields.io/hexpm/v/gl_wasm)](https://hex.pm/packages/gl_wasm)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gl_wasm/)

Create binary WebAssembly modules in Gleam.

**NOTE**: This package is currently pre-publication.

## Goals

 - Support for the WebAssembly 2.0 (draft) specification, giving priority to
 features that are useful for functional languages, such as GC and return call.
 - Make it as difficult as is reasonably possible to generate invalid
 WebAssembly.
 - Eliminate unnecessary ceremony and performance bottlenecks.

## Possible goals

 - Parsing binary WebAssembly modules.
 - Debugging support, e.g. DWARF.
 - Component Model and WASI support.

## Non-goals

 - WASM optimization.
 - Support for WAT (WebAssembly Text format).

## Progress

### Issues

 - [ ] gleb128 is incompatible with stdlib >= 0.48.0 due to bytes_builder removal - [issue](https://github.com/BrendoCosta/gleb128/issues/2)

### Features

 - [x] Building modules
 - [x] Registering types
 - [x] Building functions
 - [x] Importing functions
 - [x] Exporting functions
 - [ ] Type validation
 - [ ] Type recursion and subtyping
 - [x] Global initialization
 - [ ] Validation for global initialization
 - [ ] Advanced validation for break/return
 - [ ] Features related to unimplemented sections
 - [ ] Use bytes_tree instead of byte_array.concat & friends
 - [ ] JavaScript target support (primarily number type encoding)
 - [ ] Output stream abstraction

### Sections

 - [x] Type
 - [x] Function/code
 - [ ] Table
 - [ ] Memory
 - [x] Global
 - [ ] Element
 - [ ] Data/data count
 - [x] Start
 - [x] Export
 - [x] Import
 - [ ] Custom
 - [ ] Name

### Instructions

 - [x] General control flow
 - [x] Function calls
 - [ ] General reference instructions
 - [x] Struct
 - [ ] Array
 - [ ] i31
 - [ ] Extern
 - [ ] Table
 - [ ] Memory
 - [x] i32 and i64 encoding
 - [x] f32 and f64 encoding
 - [x] Common i32, i64, f32, f64 instructions
 - [ ] Numeric conversions, truncations, etc.
 - [ ] v128

## Usage

```sh
gleam add gl_wasm
```
```gleam
import gl_wasm

pub fn main() {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/gl_wasm>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
