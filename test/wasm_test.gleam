import gleam/bytes_tree
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleeunit/should
import ieee_float
import wasm

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
