# gl_wasm

[![Package Version](https://img.shields.io/hexpm/v/gl_wasm)](https://hex.pm/packages/gl_wasm)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/gl_wasm/)

Create binary WebAssembly modules in Gleam.

## Goals

 - Support for the [WebAssembly 3.0 (draft)
 specification](https://wasm-dsl.github.io/spectec/core/), giving priority to
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
 - [x] Type validation
 - [ ] Type recursion and subtyping
 - [x] Global initialization
 - [x] Validation for global initialization
 - [ ] Advanced validation for break/return
 - [ ] [Function-typed blocks](https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions)
 - [ ] Features related to unimplemented sections
 - [x] Use bytes_tree instead of byte_array.concat & friends
 - [x] JavaScript target support (primarily number type encoding)
 - [x] Output stream abstraction

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
 - [x] Name

### Instructions

 - [x] General control flow
 - [x] Function calls
 - [x] General reference instructions
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
import gl_wasm/wasm
import gleam/io
import gleam/list
import gleam/option.{Some}
import gleam/result
import simplifile

pub fn main() {
  case generate_wasm() {
    Error(message) -> io.println_error(message)
    Ok(_) -> Nil
  }
}

fn file_output_stream(fname) {
  let output_stream =
    wasm.OutputStream(
      stream: fname,
      write_bytes: fn(fname, bytes) {
        simplifile.append_bits(fname, bytes)
        |> result.replace(fname)
      },
      close: fn(fname) { Ok(fname) },
    )
  let _ = simplifile.write_bits(fname, <<>>)
  output_stream
}

fn generate_wasm() {
  // Create a ModuleBuilder that writes to the file "out.wasm"
  let mb = wasm.create_module_builder(Some("Doubler"))
  // Register the "add" function type and import "math.add"
  use #(mb, type_index_add) <- result.try(wasm.add_type(
    mb,
    wasm.Func(Some("add_type"), [wasm.I64, wasm.I64], [wasm.I64]),
  ))
  use mb <- result.try(wasm.import_function(
    mb,
    type_index_add,
    Some("add"),
    wasm.ImportSource("math", "add"),
  ))
  // Register the "double" function type and generate its code
  use #(mb, type_index_double) <- result.try(wasm.add_type(
    mb,
    wasm.Func(Some("double_type"), [wasm.I64], [wasm.I64]),
  ))
  use #(mb, fb) <- result.try(wasm.create_function_builder(
    mb,
    wasm.FunctionSignature(type_index_double, Some("double"), Some(["n"])),
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
  wasm.emit_module(mb, file_output_stream("out.wasm"))
  |> result.replace_error("Error writing to file")
}
```

The disassembled WebAssembly Text (WAT) representation looks like this:

```
(module $Doubler
 (type $add_type (func (param f64 f64) (result f64)))
 (type $double_type (func (param f64) (result f64)))
 (import "math" "add" (func $add (type $add_type) (param f64 f64) (result f64)))
 (export "double" (func $double))
 (func $double (type $double_type) (param $n f64) (result f64)
  (call $add
   (local.get $n)
   (local.get $n)
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
