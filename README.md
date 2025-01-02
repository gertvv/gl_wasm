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

### Features

 - [x] Building modules
 - [x] Registering types
 - [x] Building functions
 - [x] Importing functions
 - [x] Exporting functions
 - [ ] Type validation
 - [ ] Type recursion and subtyping
 - [x] Global initialization
 - [x] Validation for global initialization
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

This example generates a module that imports an "add" function and uses that to
implement a "double" function which it exports.

```gleam
import gleam/io
import gleam/list
import gleam/result
import wasm

pub fn main() {
  case generate_wasm() {
    Error(message) -> io.println_error(message)
    Ok(_) -> Nil
  }
}

fn generate_wasm() {
  // Create a ModuleBuilder that writes to the file "out.wasm"
  let mb = wasm.create_module_builder("out.wasm")
  // Register the "add" function type and import "math.add"
  use #(mb, type_index_add) <- result.try(wasm.add_type(
    mb,
    wasm.Func([wasm.I64, wasm.I64], [wasm.I64]),
  ))
  use mb <- result.try(wasm.import_function(
    mb,
    type_index_add,
    wasm.ImportSource("math", "add"),
  ))
  // Register the "double" function type and generate its code
  use #(mb, type_index_double) <- result.try(wasm.add_type(
    mb,
    wasm.Func([wasm.I64], [wasm.I64]),
  ))
  use #(mb, fb) <- result.try(wasm.create_function_builder(
    mb,
    type_index_double,
  ))
  use fb <- result.try(list.try_fold(
    over: [wasm.LocalGet(0), wasm.LocalGet(0), wasm.Call(0), wasm.End],
    from: fb,
    with: wasm.add_instruction,
  ))
  use mb <- result.try(wasm.finalize_function(mb, fb))
  // Export the "double" function
  use mb <- result.try(wasm.add_export(mb, wasm.ExportFunction("double", 1)))
  // Write the WebAssembly to file
  wasm.emit_module(mb) |> result.replace_error("Error writing to file")
}
```

The disassembled WebAssembly Text (WAT) representation looks like this:

```
(module
 (type $0 (func (param f64 f64) (result f64)))
 (type $1 (func (param f64) (result f64)))
 (import "math" "add" (func $fimport$0 (type $0) (param f64 f64) (result f64)))
 (export "double" (func $0))
 (func $0 (type $1) (param $0 f64) (result f64)
  (call $fimport$0
   (local.get $0)
   (local.get $0)
  )
 )
)
```

Further documentation can be found at <https://hexdocs.pm/gl_wasm>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```
