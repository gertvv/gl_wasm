import gleam/list
import gleam/result
import gleeunit/should
import wasm

fn simple_func(params, result, code) {
  let mb = wasm.create_module_builder("foo.wasm")
  use mb <- result.try(wasm.add_type_group(mb, [wasm.Func(params, result)]))
  use #(_, fb) <- result.try(wasm.create_function_builder(mb, 0))
  list.try_fold(code, fb, wasm.add_instruction)
}

fn simple_finalize(fb: wasm.FunctionBuilder) {
  wasm.finalize_function(fb.module_builder, fb)
  |> result.try(fn(mb) {
    list.first(mb.functions) |> result.replace_error("No functions")
  })
}

fn prepared_func(mb: wasm.ModuleBuilder, params, result, code) {
  let tidx = mb.next_type_index
  use mb <- result.try(wasm.add_type_group(mb, [wasm.Func(params, result)]))
  use #(_, fb) <- result.try(wasm.create_function_builder(mb, tidx))
  list.try_fold(code, fb, wasm.add_instruction)
}

pub fn return_const_test() {
  simple_func([], [wasm.I64], [wasm.I64Const(42), wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(
    Ok(wasm.FunctionImplementation(0, [], [wasm.I64Const(42), wasm.End])),
  )
}

pub fn finalize_incomplete_function_test() {
  simple_func([], [wasm.I64], [wasm.I64Const(42)])
  |> result.try(simple_finalize)
  |> should.equal(Error("Function incomplete"))
}

pub fn missing_return_value_test() {
  simple_func([], [wasm.I64], [wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(Error("Too few values on the stack"))
}

pub fn incorrect_return_value_test() {
  simple_func([], [wasm.F64], [wasm.I64Const(42), wasm.End])
  |> should.equal(Error("Expected f64 at depth 0 but got i64"))
}

pub fn too_many_return_values_test() {
  simple_func([], [wasm.I64], [wasm.I64Const(42), wasm.I64Const(42), wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(Error("Too many values on the stack"))
}

pub fn i64_add_test() {
  let code = [wasm.I64Const(41), wasm.I64Const(1), wasm.I64Add, wasm.End]
  simple_func([], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, [], code)))
}

pub fn i64_add_missing_arg_test() {
  simple_func([], [wasm.I64], [wasm.I64Const(1), wasm.I64Add, wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(Error("Too few values on the stack"))
}

pub fn i64_add_incorrect_arg_test() {
  simple_func([], [wasm.I64], [
    wasm.I32Const(41),
    wasm.I64Const(1),
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
  |> should.equal(Ok(wasm.FunctionImplementation(0, [], code)))
}

pub fn block_too_many_values_test() {
  simple_func([], [wasm.I64], [
    wasm.Block(wasm.BlockEmpty),
    wasm.I32Const(42),
    wasm.End,
  ])
  |> should.equal(Error("Too many values on the stack"))
}

pub fn block_result_test() {
  let code = [
    wasm.Block(wasm.BlockValue(wasm.I64)),
    wasm.I64Const(42),
    wasm.End,
    wasm.End,
  ]
  simple_func([], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, [], code)))
}

pub fn block_missing_result_test() {
  simple_func([], [wasm.I64], [wasm.Block(wasm.BlockValue(wasm.I64)), wasm.End])
  |> should.equal(Error("Too few values on the stack"))
}

pub fn local_get_test() {
  simple_func([wasm.I64], [wasm.I64], [wasm.LocalGet(0), wasm.End])
  |> result.try(simple_finalize)
  |> should.equal(
    Ok(wasm.FunctionImplementation(0, [], [wasm.LocalGet(0), wasm.End])),
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
  simple_func([wasm.I64], [], [wasm.I64Const(42), wasm.LocalSet(0), wasm.End])
  |> result.try(simple_finalize)
  |> should.be_ok
}

pub fn break_test() {
  let code = [
    wasm.Block(wasm.BlockValue(wasm.I64)),
    wasm.Block(wasm.BlockEmpty),
    wasm.I64Const(42),
    wasm.Break(1),
    wasm.End,
    wasm.I64Const(42),
    wasm.End,
    wasm.End,
  ]
  simple_func([], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, [], code)))
}

pub fn break_if_test() {
  let code = [
    wasm.Block(wasm.BlockValue(wasm.I64)),
    wasm.Block(wasm.BlockEmpty),
    wasm.I64Const(42),
    wasm.LocalGet(0),
    wasm.BreakIf(1),
    wasm.Drop,
    wasm.End,
    wasm.I64Const(0),
    wasm.End,
    wasm.End,
  ]
  simple_func([wasm.I32], [wasm.I64], code)
  |> result.try(simple_finalize)
  |> should.equal(Ok(wasm.FunctionImplementation(0, [], code)))
}

pub fn operands_outside_frame_test() {
  simple_func([], [wasm.I64], [
    wasm.I64Const(41),
    wasm.I64Const(1),
    wasm.Block(wasm.BlockEmpty),
    wasm.I64Add,
  ])
  |> should.equal(Error("Too few values on the stack"))
}

pub fn if_else_test() {
  let code = [
    wasm.LocalGet(0),
    wasm.If(wasm.BlockValue(wasm.I64)),
    wasm.I64Const(42),
    wasm.Else,
    wasm.I64Const(0),
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
    wasm.I64Const(42),
    wasm.End,
  ])
  |> should.equal(Error("Implicit else does not produce a result"))
}

pub fn if_implicit_else_without_result_test() {
  simple_func([wasm.I32], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.If(wasm.BlockEmpty),
    wasm.I64Const(42),
    wasm.Drop,
    wasm.End,
  ])
  |> should.be_ok
}

pub fn else_incorrect_type_test() {
  simple_func([wasm.I32], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.If(wasm.BlockValue(wasm.I64)),
    wasm.I64Const(42),
    wasm.Else,
    wasm.I32Const(0),
    wasm.End,
  ])
  |> should.equal(Error("Expected i64 at depth 0 but got i32"))
}

pub fn add_local_test() {
  let assert Ok(fb) = simple_func([wasm.I64], [wasm.I64], [wasm.LocalGet(0)])
  let assert Ok(#(fb, 1)) = wasm.add_local(fb, wasm.I64)
  wasm.add_instruction(fb, wasm.LocalSet(1))
  |> should.be_ok
}

pub fn add_global_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(#(mb, _gidx)) = wasm.add_global(mb, wasm.I64)
  prepared_func(mb, [], [], [wasm.GlobalGet(0)])
  |> should.be_ok
}

pub fn return_null_struct_ref_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([wasm.ValueType(wasm.Immutable, wasm.I64)]),
    ])
  let struct_ref = wasm.Ref(wasm.Nullable(wasm.ConcreteType(0)))
  prepared_func(mb, [], [struct_ref], [
    wasm.RefNull(wasm.ConcreteType(0)),
    wasm.End,
  ])
  |> result.try(simple_finalize)
  |> should.be_ok
}

pub fn return_incorrect_nullability_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([wasm.ValueType(wasm.Immutable, wasm.I64)]),
    ])
  let ref_nullable = wasm.Ref(wasm.Nullable(wasm.ConcreteType(0)))
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [ref_nullable], [ref_non_null], [wasm.LocalGet(0), wasm.End])
  |> should.equal(Error("Expected (ref $0) at depth 0 but got (ref null $0)"))
}

pub fn ref_as_non_null_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([wasm.ValueType(wasm.Immutable, wasm.I64)]),
    ])
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
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([
        wasm.ValueType(wasm.Immutable, wasm.I32),
        wasm.ValueType(wasm.Immutable, wasm.I64),
      ]),
    ])
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [], [ref_non_null], [
    wasm.I32Const(1),
    wasm.I64Const(2),
    wasm.StructNew(0),
    wasm.End,
  ])
  |> should.be_ok
}

pub fn struct_new_default_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([
        wasm.ValueType(wasm.Immutable, wasm.I32),
        wasm.ValueType(
          wasm.Immutable,
          wasm.Ref(wasm.Nullable(wasm.AbstractAny)),
        ),
      ]),
    ])
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [], [ref_non_null], [wasm.StructNewDefault(0), wasm.End])
  |> should.be_ok
}

pub fn struct_new_non_defaultable_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([
        wasm.ValueType(wasm.Immutable, wasm.I32),
        wasm.ValueType(wasm.Immutable, wasm.Ref(wasm.NonNull(wasm.AbstractAny))),
      ]),
    ])
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [], [ref_non_null], [wasm.StructNewDefault(0)])
  |> should.equal(Error("Struct type $0 has non-defaultable fields"))
}

pub fn struct_new_with_subtype_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([wasm.ValueType(wasm.Immutable, wasm.I64)]),
    ])
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([
        wasm.ValueType(wasm.Immutable, wasm.Ref(wasm.NonNull(wasm.AbstractAny))),
      ]),
    ])
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(1)))
  prepared_func(mb, [], [ref_non_null], [
    wasm.StructNewDefault(0),
    wasm.StructNew(1),
    wasm.End,
  ])
  |> should.be_ok
}

pub fn struct_get_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([wasm.ValueType(wasm.Immutable, wasm.I64)]),
    ])
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [ref_non_null], [wasm.I64], [
    wasm.LocalGet(0),
    wasm.StructGet(0, 0),
    wasm.End,
  ])
  |> result.try(simple_finalize)
  |> should.be_ok
}

pub fn struct_set_test() {
  let mb = wasm.create_module_builder("foo.wasm")
  let assert Ok(mb) =
    wasm.add_type_group(mb, [
      wasm.Struct([wasm.ValueType(wasm.Mutable, wasm.I64)]),
    ])
  let ref_non_null = wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))
  prepared_func(mb, [ref_non_null], [], [
    wasm.LocalGet(0),
    wasm.I64Const(42),
    wasm.StructSet(0, 0),
    wasm.End,
  ])
  |> result.try(simple_finalize)
  |> should.be_ok
}
