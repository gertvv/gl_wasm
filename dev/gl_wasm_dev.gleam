import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/result
import gleam/string

/// Elixir.Wasmex.StoreOrCaller
pub type StoreOrCaller

/// Elixir.Wasmex.Module
pub type Module

/// Elixir.Wasmex.Memory
pub type Memory

/// Elixir.Wasmex.Instance
pub type Instance

@external(erlang, "Elixir.Wasmex", "start_link")
pub fn start_link_ffi(opts: dict.Dict(Atom, Dynamic)) -> Result(Pid, Dynamic)

pub type Imports =
  Dict(String, Dict(String, ImportSpec))

pub fn start_link_compiled(
  store: StoreOrCaller,
  module: Module,
  imports: Imports,
  links: Dict(String, ModuleSpec),
) -> Result(Pid, String) {
  todo
}

pub fn start_link_simple(
  bytes: BitArray,
  imports: Dict(String, Dict(String, ImportSpec)),
) -> Result(Pid, String) {
  start_link_ffi(
    dict.from_list([
      #(atom.create("bytes"), dynamic.bit_array(bytes)),
      #(atom.create("imports"), wrap_imports(imports)),
    ]),
  )
  |> result.map_error(string.inspect)
}

fn wrap_imports(imports: Imports) -> Dynamic {
  dict.to_list(imports)
  |> list.map(fn(namespace) {
    let #(key, namespace) = namespace
    let namespace =
      dict.to_list(namespace)
      |> list.map(fn(import_) {
        let #(key, import_) = import_
        let import_ =
          tuple4_to_dynamic(#(
            atom.to_dynamic(atom.create("fn")),
            dynamic.list(list.map(import_.params, param_type_to_dynamic)),
            dynamic.list(list.map(import_.results, param_type_to_dynamic)),
            import_function_to_dynamic(import_.function),
          ))
        #(dynamic.string(key), import_)
      })
      |> dynamic.properties
    #(dynamic.string(key), namespace)
  })
  |> dynamic.properties
}

@external(erlang, "Elixir.Function", "identity")
fn tuple4_to_dynamic(value: #(Dynamic, Dynamic, Dynamic, Dynamic)) -> Dynamic

fn import_function_to_dynamic(function: ImportFunction) -> Dynamic {
  // TODO: must actually wrap the function to convert the context object
  // (or make the context opaque!)
  case function {
    Func1(func) -> func1_to_dynamic(func)
    Func2(func) -> func2_to_dynamic(func)
  }
}

fn param_type_to_dynamic(param_type: ParamType) -> Dynamic {
  case param_type {
    I32 -> atom.create("i32")
    I64 -> atom.create("i64")
    V128 -> atom.create("v128")
    F32 -> atom.create("f32")
    F64 -> atom.create("f64")
  }
  |> atom.to_dynamic
}

@external(erlang, "Elixir.Function", "identity")
fn func1_to_dynamic(func: fn(CallContext, Dynamic) -> Dynamic) -> Dynamic

@external(erlang, "Elixir.Function", "identity")
fn func2_to_dynamic(
  func: fn(CallContext, Dynamic, Dynamic) -> Dynamic,
) -> Dynamic

pub type ModuleSpec {
  Bytes(bytes: BitArray)
  Compiled(module: Module, links: Dict(String, ModuleSpec))
}

pub type ImportSpec {
  Func(
    params: List(ParamType),
    results: List(ParamType),
    function: ImportFunction,
  )
}

pub type ImportFunction {
  Func1(fn(CallContext, Dynamic) -> Dynamic)
  Func2(fn(CallContext, Dynamic, Dynamic) -> Dynamic)
}

pub type CallContext

pub fn context_get_memory(context: CallContext) -> Result(Memory, Nil) {
  context_fetch(context, atom.create("memory"))
  |> result.map(dynamic_as_memory)
}

pub fn context_get_caller(context: CallContext) -> Result(StoreOrCaller, Nil) {
  context_fetch(context, atom.create("caller"))
  |> result.map(dynamic_as_store_or_caller)
}

pub fn context_get_pid(context: CallContext) -> Result(Pid, Nil) {
  context_fetch(context, atom.create("pid"))
  |> result.map(dynamic_as_pid)
}

pub fn context_get_instance(context: CallContext) -> Result(Instance, Nil) {
  context_fetch(context, atom.create("instance"))
  |> result.map(dynamic_as_instance)
}

@external(erlang, "Elixir.Map", "fetch")
fn context_fetch(context: CallContext, key: Atom) -> Result(Dynamic, Nil)

@external(erlang, "Elixir.Function", "identity")
fn dynamic_as_memory(dyn: Dynamic) -> Memory

@external(erlang, "Elixir.Function", "identity")
fn dynamic_as_store_or_caller(dyn: Dynamic) -> StoreOrCaller

@external(erlang, "Elixir.Function", "identity")
fn dynamic_as_pid(dyn: Dynamic) -> Pid

@external(erlang, "Elixir.Function", "identity")
fn dynamic_as_instance(dyn: Dynamic) -> Instance

pub type ParamType {
  I32
  I64
  V128
  F32
  F64
}

pub type Number {
  NumInt(Int)
  NumFloat(Float)
}

@external(erlang, "Elixir.Wasmex", "call_function")
fn call_function_ffi(
  pid: Pid,
  name: String,
  params: Dynamic,
  timeout: Int,
) -> Result(List(Dynamic), Dynamic)

pub fn call_function(
  pid: Pid,
  name: String,
  params: List(Number),
  timeout: Int,
) -> Result(List(Number), String) {
  let decoder =
    decode.one_of(
      {
        use i <- decode.then(decode.int)
        decode.success(NumInt(i))
      },
      [
        {
          use f <- decode.then(decode.float)
          decode.success(NumFloat(f))
        },
      ],
    )
  let params =
    list.map(params, fn(param) {
      case param {
        NumInt(i) -> dynamic.int(i)
        NumFloat(f) -> dynamic.float(f)
      }
    })
  call_function_ffi(pid, name, dynamic.list(params), timeout)
  |> result.map_error(string.inspect)
  |> result.map(
    list.map(_, fn(d) {
      // Wasmex docs indicate this always returns `[number()]`
      let assert Ok(num) = decode.run(d, decoder)
      num
    }),
  )
}

@external(erlang, "Elixir.Wasmex", "store")
pub fn store(pid: Pid) -> StoreOrCaller

pub fn main() {
  //simplifile.read_bits("greet.wasm")
  let bytes =
    bit_array.from_string(
      "(module (func (export \"square\") (param i64) (result i64) local.get 0 local.get 0 i64.mul))",
    )
  let assert Ok(pid) =
    start_link_ffi(
      dict.from_list([#(atom.create("bytes"), dynamic.bit_array(bytes))]),
    )
  echo pid
  let assert Ok([NumInt(25)]) = call_function(pid, "square", [NumInt(5)], 5000)
  echo "Call a func -- OK!"

  let bytes =
    bit_array.from_string(
      "
      (module
        (import \"test\" \"add_one\" (func $add_one (param i32) (result i32)))
        (func (export \"add_two\") (param i32) (result i32) local.get 0 call $add_one call $add_one)
      )
      ",
    )

  let assert Ok(pid) =
    start_link_simple(
      bytes,
      dict.from_list([
        #(
          "test",
          dict.from_list([#("add_one", Func([I32], [I32], Func1(add_one)))]),
        ),
      ]),
    )
  let assert Ok([NumInt(8)]) = call_function(pid, "add_two", [NumInt(6)], 5000)
  echo "Import a host func -- OK!"
  // const { greet, memory } = gleamModule.instance.exports;
  // const offset = 42; // random number
  // const length = greet(offset);
  // const buffer = new Uint8Array(memory.buffer);
  // const bytes = buffer.slice(offset, offset + length);
  // console.log((new TextDecoder()).decode(bytes));
}

fn add_one(call_context: CallContext, num: Dynamic) -> Dynamic {
  let assert Ok(num) = decode.run(num, decode.int)
  dynamic.int(num + 1)
}
