import gl_wasm/wasm
import gleam/bytes_tree
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleb128
import gleeunit/should
import ieee_float

fn memory_output_stream() {
  wasm.OutputStream(
    stream: bytes_tree.new(),
    write_bytes: fn(bytes_tree, bytes) {
      Ok(bytes_tree.append(to: bytes_tree, suffix: bytes))
    },
    close: fn(s) { Ok(s) },
  )
}

fn i32_const(n: Int) -> wasm.Instruction {
  let assert Ok(int32) = wasm.int32_signed(n)
  wasm.I32Const(int32)
}

fn i64_const(n: Int) -> wasm.Instruction {
  let assert Ok(int64) = wasm.int64_signed(n)
  wasm.I64Const(int64)
}

fn simple_func(params, result, code) {
  let mb = wasm.create_module_builder(None)
  use #(mb, _) <- result.try(wasm.add_type(mb, wasm.Func(None, params, result)))
  use #(mb, fb) <- result.try(wasm.create_function_builder(
    mb,
    wasm.FunctionSignature(0, None, None),
  ))
  list.try_fold(code, fb, wasm.add_instruction)
  |> result.map(fn(fb) { #(mb, fb) })
}

fn simple_finalize(builders: #(wasm.ModuleBuilder, wasm.CodeBuilder)) {
  let #(mb, fb) = builders
  let assert wasm.BuildFunction(function_index:, ..) = wasm.builds(fb)
  wasm.finalize_function(mb, fb)
  |> result.try(wasm.get_function_by_index(_, function_index))
}

fn prepared_func(mb: wasm.ModuleBuilder, params, result, code) {
  use #(mb, tidx) <- result.try(wasm.add_type(
    mb,
    wasm.Func(None, params, result),
  ))
  use #(mb, fb) <- result.try(wasm.create_function_builder(
    mb,
    wasm.FunctionSignature(tidx, None, None),
  ))
  list.try_fold(code, fb, wasm.add_instruction)
  |> result.map(fn(fb) { #(mb, fb) })
}

pub fn return_const_test() {
  simple_func([], [wasm.I64], [i64_const(42), wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(
    Ok(wasm.FunctionImplementation(0, None, [], [], [i64_const(42), wasm.End])),
  )
}

pub fn finalize_incomplete_function_test() {
  simple_func([], [wasm.I64], [i64_const(42)])
  |> result.try(simple_finalize)
  |> should.equal(Error("Function incomplete"))
}

pub fn missing_return_value_test() {
  simple_func([], [wasm.I64], [wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(Error("Too few values on the stack"))
}

pub fn incorrect_return_value_test() {
  simple_func([], [wasm.F64], [i64_const(42), wasm.End])
  |> should.equal(Error("Expected f64 at depth 0 but got i64"))
}

pub fn too_many_return_values_test() {
  simple_func([], [wasm.I64], [i64_const(42), i64_const(42), wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(Error("Too many values on the stack"))
}

pub fn i64_add_test() {
  let code = [i64_const(41), i64_const(1), wasm.I64Add, wasm.End]
  simple_func([], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, None, [], [], code)))
}

pub fn i64_add_missing_arg_test() {
  simple_func([], [wasm.I64], [i64_const(1), wasm.I64Add, wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(Error("Too few values on the stack"))
}

pub fn i64_add_incorrect_arg_test() {
  simple_func([], [wasm.I64], [
    i32_const(41),
    i64_const(1),
    wasm.I64Add,
    wasm.End,
  ])
  |> result.try(simple_finalize)
  |> should.equal(Error("Expected i64 at depth 1 but got i32"))
}

pub fn block_empty_test() {
  let code = [wasm.Block(wasm.BlockEmpty), wasm.End, wasm.End]
  simple_func([], [], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, None, [], [], code)))
}

pub fn block_too_many_values_test() {
  simple_func([], [wasm.I64], [
    wasm.Block(wasm.BlockEmpty),
    i32_const(42),
    wasm.End,
  ])
  |> should.equal(Error("Too many values on the stack"))
}

pub fn block_result_test() {
  let code = [
    wasm.Block(wasm.BlockValue(wasm.I64)),
    i64_const(42),
    wasm.End,
    wasm.End,
  ]
  simple_func([], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, None, [], [], code)))
}

pub fn block_missing_result_test() {
  simple_func([], [wasm.I64], [wasm.Block(wasm.BlockValue(wasm.I64)), wasm.End])
  |> should.equal(Error("Too few values on the stack"))
}

pub fn local_get_test() {
  simple_func([wasm.I64], [wasm.I64], [wasm.LocalGet(0), wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(
    Ok(
      wasm.FunctionImplementation(0, None, [None], [], [
        wasm.LocalGet(0),
        wasm.End,
      ]),
    ),
  )
}

pub fn local_get_oob_test() {
  simple_func([wasm.I64], [wasm.I64], [wasm.LocalGet(1)])
  |> result.try(simple_finalize)
  |> should.equal(Error("Local index 1 does not exist"))
}

pub fn local_get_incorrect_type_test() {
  simple_func([wasm.I32], [wasm.I64], [wasm.LocalGet(0), wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(Error("Expected i64 at depth 0 but got i32"))
}

pub fn local_set_test() {
  simple_func([wasm.I64], [], [i64_const(42), wasm.LocalSet(0), wasm.End])
  |> result.try(simple_finalize)
  |> should.be_ok
}

pub fn break_test() {
  let code = [
    wasm.Block(wasm.BlockValue(wasm.I64)),
    wasm.Block(wasm.BlockEmpty),
    i64_const(42),
    wasm.Break(1),
    wasm.End,
    i64_const(42),
    wasm.End,
    wasm.End,
  ]
  simple_func([], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, None, [], [], code)))
}

pub fn break_if_test() {
  let code = [
    wasm.Block(wasm.BlockValue(wasm.I64)),
    wasm.Block(wasm.BlockEmpty),
    i64_const(42),
    wasm.LocalGet(0),
    wasm.BreakIf(1),
    wasm.Drop,
    wasm.End,
    i64_const(0),
    wasm.End,
    wasm.End,
  ]
  simple_func([wasm.I32], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, None, [None], [], code)))
}

pub fn operands_outside_frame_test() {
  simple_func([], [wasm.I64], [
    i64_const(41),
    i64_const(1),
    wasm.Block(wasm.BlockEmpty),
    wasm.I64Add,
  ])
  |> should.equal(Error("Too few values on the stack"))
}

pub fn if_else_test() {
  let code = [
    wasm.LocalGet(0),
    wasm.If(wasm.BlockValue(wasm.I64)),
    i64_const(42),
    wasm.Else,
    i64_const(0),
    wasm.End,
    wasm.End,
  ]
  simple_func([wasm.I32], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.be_ok
}

pub fn if_implicit_else_with_result_test() {
  simple_func([wasm.I32], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.If(wasm.BlockValue(wasm.I64)),
    i64_const(42),
    wasm.End,
  ])
  |> should.equal(Error("Implicit else does not produce a result"))
}

pub fn if_implicit_else_without_result_test() {
  simple_func([wasm.I32], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.If(wasm.BlockEmpty),
    i64_const(42),
    wasm.Drop,
    wasm.End,
  ])
  |> should.be_ok
}

pub fn else_incorrect_type_test() {
  simple_func([wasm.I32], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.If(wasm.BlockValue(wasm.I64)),
    i64_const(42),
    wasm.Else,
    i32_const(0),
    wasm.End,
  ])
  |> should.equal(Error("Expected i64 at depth 0 but got i32"))
}

pub fn add_local_test() {
  let assert Ok(#(_mb, fb)) =
    simple_func([wasm.I64], [wasm.I64], [wasm.LocalGet(0)])
  let assert Ok(#(fb, 1)) = wasm.add_local(fb, wasm.I64, None)
  wasm.add_instruction(fb, wasm.LocalSet(1))
  |> should.be_ok
}

pub fn add_global_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, gb)) =
    wasm.create_global_builder(mb, None, wasm.Mutable, wasm.I64)
  let assert Ok(gb) = wasm.add_instruction(gb, i64_const(12))
  let assert Ok(gb) = wasm.add_instruction(gb, wasm.End)
  let assert Ok(mb) = wasm.finalize_global(mb, gb)
  prepared_func(mb, [], [], [wasm.GlobalGet(0)])
  |> should.be_ok
}

pub fn add_global_non_const_expr_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(_mb, gb)) =
    wasm.create_global_builder(mb, None, wasm.Mutable, wasm.I64)
  let assert Ok(gb) = wasm.add_instruction(gb, i64_const(12))
  let assert Ok(gb) = wasm.add_instruction(gb, i64_const(12))
  wasm.add_instruction(gb, wasm.I64Add)
  |> should.equal(Error(
    "Only constant expressions are allowed in global initializers",
  ))
}

pub fn return_null_struct_ref_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [wasm.ValueType(None, wasm.Immutable, wasm.I64)]),
    )
  let struct_ref = wasm.Ref(wasm.Nullable(wasm.ConcreteType(0)))
  prepared_func(mb, [], [struct_ref], [
    wasm.RefNull(wasm.ConcreteType(0)),
    wasm.End,
  ])
  |> result.try(simple_finalize)
  |> should.be_ok
}

pub fn return_incorrect_nullability_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [wasm.ValueType(None, wasm.Immutable, wasm.I64)]),
    )
  let ref_nullable = wasm.Ref(wasm.Nullable(wasm.ConcreteType(0)))
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [ref_nullable], [ref_non_null], [wasm.LocalGet(0), wasm.End])
  |> should.equal(Error("Expected (ref $0) at depth 0 but got (ref null $0)"))
}

pub fn ref_as_non_null_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [wasm.ValueType(None, wasm.Immutable, wasm.I64)]),
    )
  let ref_nullable = wasm.Ref(wasm.Nullable(wasm.ConcreteType(0)))
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [ref_nullable], [ref_non_null], [
    wasm.LocalGet(0),
    wasm.RefAsNonNull,
    wasm.End,
  ])
  |> should.be_ok
}

pub fn struct_new_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [
        wasm.ValueType(None, wasm.Immutable, wasm.I32),
        wasm.ValueType(None, wasm.Immutable, wasm.I64),
      ]),
    )
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [], [ref_non_null], [
    i32_const(1),
    i64_const(2),
    wasm.StructNew(0),
    wasm.End,
  ])
  |> should.be_ok
}

pub fn struct_subtype_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_sub_type_group(mb, [
      wasm.SubOpen([], wasm.Struct(None, [])),
      wasm.SubFinal(
        [0],
        wasm.Struct(None, [wasm.ValueType(None, wasm.Immutable, wasm.I32)]),
      ),
    ])
  let ref_non_null_0 = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [], [ref_non_null_0], [
    i32_const(1),
    wasm.StructNew(1),
    wasm.End,
  ])
  |> should.be_ok
}

pub fn struct_new_default_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [
        wasm.ValueType(None, wasm.Immutable, wasm.I32),
        wasm.ValueType(
          None,
          wasm.Immutable,
          wasm.Ref(wasm.Nullable(wasm.AbstractAny)),
        ),
      ]),
    )
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [], [ref_non_null], [wasm.StructNewDefault(0), wasm.End])
  |> should.be_ok
}

pub fn struct_new_non_defaultable_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [
        wasm.ValueType(None, wasm.Immutable, wasm.I32),
        wasm.ValueType(
          None,
          wasm.Immutable,
          wasm.Ref(wasm.NonNull(wasm.AbstractAny)),
        ),
      ]),
    )
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [], [ref_non_null], [wasm.StructNewDefault(0)])
  |> should.equal(Error("Struct type $0 has non-defaultable fields"))
}

pub fn struct_new_with_subtype_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [wasm.ValueType(None, wasm.Immutable, wasm.I64)]),
    )
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [
        wasm.ValueType(
          None,
          wasm.Immutable,
          wasm.Ref(wasm.NonNull(wasm.AbstractAny)),
        ),
      ]),
    )
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(1)))
  prepared_func(mb, [], [ref_non_null], [
    wasm.StructNewDefault(0),
    wasm.StructNew(1),
    wasm.End,
  ])
  |> should.be_ok
}

pub fn struct_get_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [wasm.ValueType(None, wasm.Immutable, wasm.I64)]),
    )
  let ref_nullable = wasm.Ref(wasm.Nullable(wasm.ConcreteType(0)))
  prepared_func(mb, [ref_nullable], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.StructGet(0, 0),
    wasm.End,
  ])
  |> result.try(simple_finalize)
  |> should.be_ok
}

pub fn struct_get_non_struct_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [wasm.ValueType(None, wasm.Immutable, wasm.I64)]),
    )
  prepared_func(mb, [wasm.I64], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.StructGet(0, 0),
  ])
  |> should.equal(Error("Expected (ref null $0) at depth 0 but got i64"))
}

pub fn struct_set_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [wasm.ValueType(None, wasm.Mutable, wasm.I64)]),
    )
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [ref_non_null], [], [
    wasm.LocalGet(0),
    i64_const(42),
    wasm.StructSet(0, 0),
    wasm.End,
  ])
  |> result.try(simple_finalize)
  |> should.be_ok
}

pub fn struct_set_immutable_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [wasm.ValueType(None, wasm.Immutable, wasm.I64)]),
    )
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [ref_non_null], [], [
    wasm.LocalGet(0),
    i64_const(42),
    wasm.StructSet(0, 0),
  ])
  |> should.equal(Error("struct.set on immutable field"))
}

pub fn func_call_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(mb, wasm.Func(None, [wasm.I64, wasm.F64], [wasm.F64]))
  let assert Ok(#(mb, _fb)) =
    wasm.create_function_builder(mb, wasm.FunctionSignature(0, None, None))
  let assert Ok(#(mb, fb)) =
    prepared_func(mb, [], [wasm.F64], [
      i64_const(42),
      wasm.F64Const(ieee_float.finite(3.14)),
      wasm.Call(0),
      wasm.End,
    ])
  wasm.finalize_function(mb, fb)
  |> should.be_ok
}

pub fn func_call_ref_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(mb, wasm.Func(None, [wasm.I64, wasm.F64], [wasm.F64]))
  let assert Ok(#(mb, _fb)) =
    wasm.create_function_builder(mb, wasm.FunctionSignature(0, None, None))
  let assert Ok(#(mb, fb)) =
    prepared_func(
      mb,
      [wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))],
      [wasm.F64],
      [
        i64_const(42),
        wasm.F64Const(ieee_float.finite(3.14)),
        wasm.LocalGet(0),
        wasm.CallRef(0),
        wasm.End,
      ],
    )
  wasm.finalize_function(mb, fb)
  |> should.be_ok
}

pub fn import_function_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(mb, wasm.Func(None, [wasm.I64, wasm.F64], [wasm.F64]))
  let from = wasm.ImportSource("some_module", "some_function")
  let assert Ok(mb) = wasm.import_function(mb, 0, None, from)
  wasm.get_function_by_index(mb, 0)
  |> should.equal(Ok(wasm.FunctionImport(type_index: 0, name: None, from:)))
}

pub fn import_non_function_type_test() {
  let mb = wasm.create_module_builder(None)
  let assert Ok(#(mb, _)) =
    wasm.add_type(
      mb,
      wasm.Struct(None, [
        wasm.ValueType(None, wasm.Immutable, wasm.I64),
        wasm.ValueType(None, wasm.Immutable, wasm.F64),
      ]),
    )
  let from = wasm.ImportSource("some_module", "some_function")
  wasm.import_function(mb, 0, None, from)
  |> should.equal(Error("Type $0 is not a func"))
}

pub fn add_type_refer_undefined_type_test() {
  let mb = wasm.create_module_builder(None)
  wasm.add_type(
    mb,
    wasm.Struct(None, [
      wasm.ValueType(
        None,
        wasm.Immutable,
        wasm.Ref(wasm.NonNull(wasm.ConcreteType(1))),
      ),
    ]),
  )
  |> should.equal(Error("Type $1 not defined"))
}

pub fn add_type_group_refer_sibling_test() {
  let mb = wasm.create_module_builder(None)
  wasm.add_type_group(mb, [
    wasm.Struct(None, [
      wasm.ValueType(
        None,
        wasm.Immutable,
        wasm.Ref(wasm.NonNull(wasm.ConcreteType(1))),
      ),
    ]),
    wasm.Func(None, [wasm.I64], [wasm.I64]),
  ])
  |> should.be_ok
}

const magic = <<0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00>>

pub fn emit_empty_module_test() {
  let mb = wasm.create_module_builder(None)
  wasm.emit_module(mb, memory_output_stream())
  |> result.map(bytes_tree.to_bit_array)
  |> should.equal(Ok(magic))
}

pub fn emit_basic_function_test() {
  // sec id = 1, size = 7 bytes, len = 1
  let type_sec_header = <<0x01, 0x07, 0x01>>
  // 0x60, n = 2 params, i64, i64, n = 1 result, i64 
  let func_type = <<0x60, 0x02, 0x7e, 0x7e, 0x01, 0x7e>>
  // sec id = 3, size = 2, len = 1, type id = 0
  let func_sec = <<0x03, 0x02, 0x01, 0x00>>
  // sec id = 7, size = 8 bytes, len = 1
  let export_sec_header = <<0x07, 0x08, 0x01>>
  // export len = 4, "plus", func (0), id = 0
  let func_export = <<0x04, "plus":utf8, 0x00, 0x00>>
  // sec id = 10, size = 9 bytes, len = 1
  let code_sec_header = <<0x0a, 0x09, 0x01>>
  // size = 7 bytes, 0 locals, local.get 0, local.get 1, i64.add, end
  let func_code = <<0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0x7c, 0x0b>>

  simple_func([wasm.I64, wasm.I64], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.LocalGet(1),
    wasm.I64Add,
    wasm.End,
  ])
  |> result.try(fn(pair) {
    let #(mb, fb) = pair
    wasm.finalize_function(mb, fb)
  })
  |> result.try(wasm.add_export(_, wasm.ExportFunction("plus", 0)))
  |> result.map_error(wasm.ValidationError)
  |> result.try(wasm.emit_module(_, memory_output_stream()))
  |> result.map(bytes_tree.to_bit_array)
  |> should.equal(
    Ok(<<
      magic:bits,
      type_sec_header:bits,
      func_type:bits,
      func_sec:bits,
      export_sec_header:bits,
      func_export:bits,
      code_sec_header:bits,
      func_code:bits,
    >>),
  )
}

pub fn emit_basic_function_with_names_test() {
  // type, func, code (as previous test but s/i64/f64/ and without the export)
  let regular_sections = <<
    0x01, 0x07, 0x01, 0x60, 0x02, 0x7c, 0x7c, 0x01, 0x7c, 0x03, 0x02, 0x01, 0x00,
    0x0a, 0x09, 0x01, 0x07, 0x00, 0x20, 0x00, 0x20, 0x01, 0xa0, 0x0b,
  >>
  // sec id = 0, size = 38 bytes(?), len = 4, "name"
  let name_sec_header = <<0x00, 0x26, 0x04, "name":utf8>>
  // module subsection: id = 0, size = 8 bytes, len = 7, "add_f64"
  let name_sub_module = <<0x00, 0x08, 0x07, "add_f64":utf8>>
  // function subsection: id = 1, size = 4 bytes, 1 assoc, func id 0, len = 1, "f"
  let name_sub_function = <<0x01, 0x04, 0x01, 0x00, 0x01, "f":utf8>>
  // local subsection: id = 2, size = 9, 1 assoc, func id 0, 2 assocs, local 0, len = 1, "x"
  let name_sub_locals = <<
    0x02, 0x09, 0x01, 0x00, 0x02, 0x00, 0x01, "x":utf8, 0x01, 0x01, "y":utf8,
  >>
  // type subsection: id = 4, size = 4, 1 assoc, type id 0, len = 1, "t"
  let name_sub_types = <<0x04, 0x04, 0x01, 0x00, 0x01, "t":utf8>>

  let mb = wasm.create_module_builder(Some("add_f64"))
  wasm.add_type(mb, wasm.Func(Some("t"), [wasm.F64, wasm.F64], [wasm.F64]))
  |> result.map(fn(res) {
    let #(mb, _index) = res
    mb
  })
  |> result.try(wasm.create_function_builder(
    _,
    wasm.FunctionSignature(0, Some("f"), Some(["x", "y"])),
  ))
  |> result.try(fn(res) {
    let #(mb, fb) = res
    list.try_fold(
      [wasm.LocalGet(0), wasm.LocalGet(1), wasm.F64Add, wasm.End],
      fb,
      wasm.add_instruction,
    )
    |> result.try(wasm.finalize_function(mb, _))
  })
  |> result.map_error(wasm.ValidationError)
  |> result.try(wasm.emit_module(_, memory_output_stream()))
  |> result.map(bytes_tree.to_bit_array)
  |> should.equal(
    Ok(<<
      magic:bits,
      regular_sections:bits,
      name_sec_header:bits,
      name_sub_module:bits,
      name_sub_function:bits,
      name_sub_locals:bits,
      name_sub_types:bits,
    >>),
  )
}

pub fn emit_locals_and_blocks_test() {
  let assert Ok(zero) = wasm.int32_unsigned(0)
  let assert Ok(one) = wasm.int32_unsigned(1)
  let mb = wasm.create_module_builder(None)
  wasm.add_type(mb, wasm.Func(None, [wasm.I32], [wasm.I32]))
  |> result.map(pair.first)
  |> result.try(wasm.create_function_builder(
    _,
    wasm.FunctionSignature(0, None, None),
  ))
  |> result.try(fn(res) {
    // Add two locals
    let #(mb, fb) = res
    wasm.add_local(fb, wasm.I32, None)
    |> result.map(pair.first)
    |> result.try(wasm.add_local(_, wasm.I32, None))
    |> result.map(pair.first)
    |> result.map(pair.new(mb, _))
  })
  |> result.try(fn(res) {
    let #(mb, fb) = res
    list.try_fold(
      [
        wasm.I32Const(zero),
        wasm.LocalSet(1),
        wasm.I32Const(one),
        wasm.LocalSet(2),
        wasm.Block(wasm.BlockEmpty),
        wasm.Loop(wasm.BlockEmpty),
        wasm.LocalGet(0),
        wasm.I32EqZ,
        wasm.BreakIf(1),
        wasm.LocalGet(2),
        wasm.LocalGet(1),
        wasm.LocalGet(2),
        wasm.I32Add,
        wasm.LocalSet(2),
        wasm.LocalSet(1),
        wasm.LocalGet(0),
        wasm.I32Const(one),
        wasm.I32Sub,
        wasm.LocalSet(0),
        wasm.Break(0),
        wasm.End,
        wasm.Unreachable,
        wasm.End,
        wasm.LocalGet(1),
        wasm.End,
      ],
      fb,
      wasm.add_instruction,
    )
    |> result.try(wasm.finalize_function(mb, _))
  })
  |> result.try(wasm.add_export(_, wasm.ExportFunction("fib", 0)))
  |> result.map_error(wasm.ValidationError)
  |> result.try(wasm.emit_module(_, memory_output_stream()))
  |> result.map(bytes_tree.to_bit_array)
  |> result.map(section_content(_, 10))
  |> should.equal(
    Ok(<<
      // one func, 46 bytes, locals = 2xi32
      0x01, 0x2e, 0x01, 0x02, 0x7f,
      // i32.const 0, set 1, i32.const 1, set 2
      0x41, 0x00, 0x21, 0x01, 0x41, 0x01, 0x21, 0x02,
      // block empty, loop empty, get 0, i32.eqz, br_if 1
      0x02, 0x40, 0x03, 0x40, 0x20, 0x00, 0x45, 0x0d, 0x01,
      // get 2, get 1, get 2, i32.add, set 2, set 1
      0x20, 0x02, 0x20, 0x01, 0x20, 0x02, 0x6a, 0x21, 0x02, 0x21, 0x01,
      // get 0, i32.const 1, i32.sub, set 0
      0x20, 0x00, 0x41, 0x01, 0x6b, 0x21, 0x00,
      // break 0, end, unreachable, end, get 1, end
      0x0c, 0x00, 0x0b, 0x00, 0x0b, 0x20, 0x01, 0x0b,
    >>),
  )
}

fn section_content(wasm: BitArray, section_id: Int) -> BitArray {
  case wasm {
    <<header:size(64)-bits, rest:bits>> if header == magic ->
      do_section_content(rest, section_id)
    _ -> <<>>
  }
}

fn do_section_content(wasm: BitArray, section_id: Int) -> BitArray {
  case wasm {
    <<id:size(8), rest:bits>> if id >= 0 && id <= 12 -> {
      gleb128.decode_unsigned(rest)
      |> result.map(fn(size) {
        let #(bytes, count) = size
        let bits = bytes * 8
        let count_bits = count * 8
        case rest {
          <<_size:size(count_bits)-bits, content:size(bits)-bits, rest:bits>> -> {
            case id == section_id {
              True -> content
              False -> do_section_content(rest, section_id)
            }
          }
          _ -> <<>>
        }
      })
      |> result.unwrap(<<>>)
    }
    _ -> <<>>
  }
}
