//// Generate binary WebAssembly modules in pure Gleam.
////
//// WebAssembly types and instructions are represented by Gleam types and
//// validation is performed as the WebAssembly is generated.
////
//// ## Example
//// ```
//// let mb = create_module_builder("out.wasm")
//// ...
//// ```
//// Runs a simple example. To be removed.

import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleb128
import simplifile

pub fn main() {
  impl() |> io.debug
}

fn impl() {
  // TODO: implement add/fold/sum
  let mb = create_module_builder("out.wasm")
  use mb <- result.try(
    add_type_group(mb, [
      Struct([ValueType(Immutable, I64), ValueType(Immutable, I64)]),
    ]),
  )
  use mb <- result.try(
    add_type_group(mb, [Func([Ref(NonNull(ConcreteType(0)))], [I64])]),
  )
  use #(mb, fb) <- result.try(create_function_builder(mb, 1))
  use fb <- result.try(list.try_fold(
    over: [LocalGet(0), StructGet(0, 0), End],
    from: fb,
    with: add_instruction,
  ))
  use mb <- result.try(finalize_function(mb, fb))
  emit_module(mb) |> result.replace_error("Error writing to file")
}

// --------------------------------------------------------------------------- 
// Type definitions
// --------------------------------------------------------------------------- 

pub type ModuleBuilder {
  ModuleBuilder(
    output_file_path: String,
    /// type groups stored in reverse order
    types: List(List(CompositeType)),
    next_type_index: Int,
    /// functions stored in reverse order
    functions: List(FunctionDefinition),
    next_function_index: Int,
    /// globals stored in reverse order
    globals: List(ValueType),
    next_global_index: Int,
  )
}

pub type FunctionBuilder {
  FunctionBuilder(
    module_builder: ModuleBuilder,
    function_index: Int,
    type_index: Int,
    params: List(ValueType),
    result: List(ValueType),
    locals: List(ValueType),
    next_local_index: Int,
    code: List(Instruction),
    value_stack: List(ValueType),
    label_stack: List(Label),
  )
}

pub type FunctionDefinition {
  FunctionImport(type_index: Int, Import)
  FunctionImplementation(
    type_index: Int,
    locals: List(ValueType),
    code: List(Instruction),
  )
  FunctionMissing(type_index: Int)
}

pub type Import {
  Import(module: String, name: String)
}

/// Value types are the types that a variable accepts
pub type ValueType {
  I32
  I64
  F32
  F64
  V128
  Ref(RefType)
}

/// Reference type
pub type RefType {
  NonNull(HeapType)
  Nullable(HeapType)
}

/// Heap types classify the objects in the runtime store.
/// They may be abstract or concrete
pub type HeapType {
  AbstractFunc
  AbstractNoFunc
  AbstractExtern
  AbstractNoExtern
  AbstractAny
  AbstractEq
  AbstractI31
  AbstractStruct
  AbstractArray
  AbstractNone
  ConcreteType(Int)
}

/// Composite types are those composed from simpler types
pub type CompositeType {
  Func(List(ValueType), List(ValueType))
  Array(FieldType)
  Struct(List(FieldType))
}

/// Field types describe the components of aggregate types (arrays and
/// structs).
pub type FieldType {
  PackedType(mutable: Mutability, PackedType)
  ValueType(mutable: Mutability, ValueType)
}

/// Indicates wheter a field is mutable
pub type Mutability {
  Mutable
  Immutable
}

/// Packed types cannot exist as standalone values but can be fields
pub type PackedType {
  I8
  I16
}

/// Labels keep track of the scope (block/frame)
pub type Label {
  LabelFunc(stack_limit: Int, result: List(ValueType))
  LabelBlock(stack_limit: Int, result: List(ValueType))
  LabelLoop(stack_limit: Int, result: List(ValueType))
  LabelIf(stack_limit: Int, result: List(ValueType))
  LabelElse(stack_limit: Int, result: List(ValueType))
}

/// WebAssembly instructions (code)
pub type Instruction {
  // Control instructions
  Unreachable
  Nop
  Block(block_type: BlockType)
  Loop(block_type: BlockType)
  If(block_type: BlockType)
  Else
  Break(label_index: Int)
  BreakIf(label_index: Int)
  // TODO: BreakTable
  Return
  Call(function_index: Int)
  // TODO: CallIndirect
  ReturnCall(function_index: Int)
  // TODO: ReturnCallIndirect
  CallRef(type_index: Int)
  ReturnCallRef(type_index: Int)
  // TODO: BreakOn*
  End
  //
  // Reference instructions
  RefNull(heap_type: HeapType)
  RefFunc(function_index: Int)
  RefIsNull
  RefAsNonNull
  RefEq
  RefTest(ref_type: RefType)
  RefCast(ref_type: RefType)
  StructNew(type_index: Int)
  StructNewDefault(type_index: Int)
  StructGet(type_index: Int, field_index: Int)
  StructGetS(type_index: Int, field_index: Int)
  StructGetU(type_index: Int, field_index: Int)
  StructSet(type_index: Int, field_index: Int)
  // TODO: remaining reference instructions
  //
  // Parametric instructions
  Drop
  Select(List(ValueType))
  //
  // Variable instructions
  LocalGet(local_index: Int)
  LocalSet(local_index: Int)
  LocalTee(local_index: Int)
  GlobalGet(global_index: Int)
  GlobalSet(global_index: Int)
  //
  // Table instructions
  // TODO: table instructions
  //
  // Memory instructions
  // TODO: memory instructions
  //
  // Numeric instructions
  // TODO: should some of the *Const constructions take a BitArray?
  I32Const(value: Int)
  I64Const(value: Int)
  F32Const(value: Float)
  F64Const(value: Float)
  I32EqZ
  I32Eq
  I32NE
  I32LtS
  I32LtU
  I32GtS
  I32GtU
  I32LeS
  I32LeU
  I32GeS
  I32GeU
  I64EqZ
  I64Eq
  I64NE
  I64LtS
  I64LtU
  I64GtS
  I64GtU
  I64LeS
  I64LeU
  I64GeS
  I64GeU
  F32Eq
  F32NE
  F32Lt
  F32Gt
  F32Le
  F32Ge
  F64Eq
  F64NE
  F64Lt
  F64Gt
  F64Le
  F64Ge
  I32CntLZ
  I32CntTZ
  I32PopCnt
  I32Add
  I32Sub
  I32Mul
  I32DivS
  I32DivU
  I32RemS
  I32RemU
  I32And
  I32Or
  I32Xor
  I32ShL
  I32ShRS
  I32ShLU
  I32RotL
  I32RotR
  I64CntLZ
  I64CntTZ
  I64PopCnt
  I64Add
  I64Sub
  I64Mul
  I64DivS
  I64DivU
  I64RemS
  I64RemU
  I64And
  I64Or
  I64Xor
  I64ShL
  I64ShRS
  I64ShLU
  I64RotL
  I64RotR
  F32Abs
  F32Neg
  F32Ceil
  F32Floor
  F32Trunc
  F32Nearest
  F32Sqrt
  F32Add
  F32Sub
  F32Mul
  F32Div
  F32Min
  F32Max
  F32CopySign
  F64Abs
  F64Neg
  F64Ceil
  F64Floor
  F64Trunc
  F64Nearest
  F64Sqrt
  F64Add
  F64Sub
  F64Mul
  F64Div
  F64Min
  F64Max
  F64CopySign
  // TODO: conversions / truncations / etc.
  //
  // Vector instructions
  // TODO: vector instructions
}

/// BlockType represents the value produced by a block (or empty)
pub type BlockType {
  BlockEmpty
  BlockValue(ValueType)
}

// --------------------------------------------------------------------------- 
// WebAssembly module & code builders
// --------------------------------------------------------------------------- 

/// Creates a ModuleBuilder that outputs to a specific file path.
///
/// In a future version, the file path will likely be replaced by an output
/// stream abstraction.
pub fn create_module_builder(output_file_path: String) -> ModuleBuilder {
  ModuleBuilder(
    output_file_path:,
    types: [],
    next_type_index: 0,
    functions: [],
    next_function_index: 0,
    globals: [],
    next_global_index: 0,
  )
}

fn get_type_by_index(
  mb: ModuleBuilder,
  index: Int,
) -> Result(CompositeType, String) {
  case index < mb.next_type_index {
    True -> get_type_by_index_loop(mb.types, mb.next_type_index - index)
    False -> Error("Type index out of bounds")
  }
}

fn get_type_by_index_loop(type_groups, index) -> Result(CompositeType, String) {
  case type_groups {
    [] -> Error("Could not get type by index")
    [group, ..rest] -> {
      let length = list.length(group)
      case length < index {
        True -> get_type_by_index_loop(rest, index - length)
        False ->
          list_index(group, index - 1)
          |> result.replace_error("Could not get type by index")
      }
    }
  }
}

fn list_index(lst: List(a), idx: Int) -> Result(a, Nil) {
  case lst, idx {
    [head, ..], 0 -> Ok(head)
    [_, ..tail], n if n > 0 -> list_index(tail, n - 1)
    _, _ -> Error(Nil)
  }
}

fn list_replace(lst: List(a), idx: Int, val: a) -> Result(List(a), Nil) {
  // TODO: tail recursive
  case lst, idx {
    [_head, ..tail], 0 -> Ok([val, ..tail])
    [head, ..tail], n if n > 0 ->
      list_replace(tail, n - 1, val)
      |> result.map(fn(new_tail) { [head, ..new_tail] })
    _, _ -> Error(Nil)
  }
}

/// Adds a type recursion group to the module.
///
/// Can be done any time before creating a function that uses it.
pub fn add_type_group(
  mb: ModuleBuilder,
  group: List(CompositeType),
) -> Result(ModuleBuilder, String) {
  // TODO: validation
  Ok(
    ModuleBuilder(
      ..mb,
      types: [group, ..mb.types],
      next_type_index: mb.next_type_index + list.length(group),
    ),
  )
}

fn get_type(fb: FunctionBuilder, type_index: Int) {
  get_type_by_index(fb.module_builder, type_index)
}

/// Register a function placeholder with the `ModuleBuilder` and create a
/// FunctionBuilder for it.
///
/// Multiple function implementations can be generated in parallel, to be
/// finalized into the `ModuleBuilder` when ready.
pub fn create_function_builder(
  mb: ModuleBuilder,
  type_index: Int,
) -> Result(#(ModuleBuilder, FunctionBuilder), String) {
  case get_type_by_index(mb, type_index) {
    Ok(Func(params, result)) -> {
      let module_builder =
        ModuleBuilder(
          ..mb,
          functions: [FunctionMissing(type_index)],
          next_function_index: mb.next_type_index + 1,
        )
      Ok(#(
        module_builder,
        FunctionBuilder(
          module_builder:,
          function_index: mb.next_function_index,
          type_index:,
          params:,
          result:,
          locals: [],
          next_local_index: list.length(params),
          code: [],
          value_stack: [],
          label_stack: [LabelFunc(0, result)],
        ),
      ))
    }
    _ -> Error("No func type at this index")
  }
}

fn get_function(
  fb: FunctionBuilder,
  function_index: Int,
) -> Result(FunctionDefinition, String) {
  let mb = fb.module_builder
  list_index(mb.functions, mb.next_function_index - function_index)
  |> result.replace_error(
    "Function index " <> int.to_string(function_index) <> " does not exist",
  )
}

/// Add a global to the `ModuleBuilder`.
///
/// Can be done any time before creating a function that uses it.
pub fn add_global(
  mb: ModuleBuilder,
  t: ValueType,
) -> Result(#(ModuleBuilder, Int), String) {
  Ok(#(
    ModuleBuilder(
      ..mb,
      globals: [t, ..mb.globals],
      next_global_index: mb.next_global_index + 1,
    ),
    mb.next_global_index,
  ))
}

fn get_global(
  fb: FunctionBuilder,
  global_index: Int,
) -> Result(ValueType, String) {
  let mb = fb.module_builder
  list_index(mb.globals, { mb.next_global_index - 1 } - global_index)
  |> result.replace_error(
    "Global index " <> int.to_string(global_index) <> " does not exist",
  )
}

/// Add an instruction to the `FunctionBuilder`.
///
/// Validates the stack contains the value types the instruction requires.
pub fn add_instruction(
  fb: FunctionBuilder,
  instr: Instruction,
) -> Result(FunctionBuilder, String) {
  use #(top_label, bottom_labels) <- result.try(case fb.label_stack {
    [] -> Error("Attempted to add instruction to ended function")
    [head, ..tail] -> Ok(#(head, tail))
  })
  case instr {
    Unreachable -> {
      // unreachable clears the frame stack and produces the desired results
      let value_stack =
        list.drop(
          fb.value_stack,
          list.length(fb.value_stack) - top_label.stack_limit,
        )
        |> list.fold(over: top_label.result, from: _, with: list.prepend)
      Ok(FunctionBuilder(..fb, value_stack:))
    }
    Nop -> Ok(fb)
    Block(block_type) | Loop(block_type) | If(block_type) -> {
      let result = case block_type {
        BlockEmpty -> []
        BlockValue(v) -> [v]
      }
      use #(fb, label_maker) <- result.map(case instr {
        Block(_) -> Ok(#(fb, LabelBlock))
        Loop(_) -> Ok(#(fb, LabelLoop))
        _ -> pop_push(fb, [I32], []) |> result.map(fn(fb) { #(fb, LabelIf) })
      })
      FunctionBuilder(
        ..fb,
        label_stack: [
          label_maker(list.length(fb.value_stack), result),
          ..fb.label_stack
        ],
      )
    }
    Else -> {
      use new_label <- result.try(case top_label {
        LabelIf(stack_limit:, result:) -> Ok(LabelElse(stack_limit:, result:))
        _ -> Error("Else must follow If")
      })
      check_stack_top_exact(fb, top_label.result)
      |> result.map(fn(fb) {
        FunctionBuilder(
          ..fb,
          value_stack: list.drop(fb.value_stack, list.length(top_label.result)),
          label_stack: [new_label, ..bottom_labels],
        )
      })
    }
    Break(label_index) -> {
      use break_to <- result.try(
        list_index(fb.label_stack, label_index)
        |> result.replace_error("Too few labels on the stack"),
      )
      pop_push(fb, break_to.result, [])
    }
    BreakIf(label_index) -> {
      use break_to <- result.try(
        list_index(fb.label_stack, label_index)
        |> result.replace_error("Too few labels on the stack"),
      )
      use fb <- result.try(pop_push(fb, [I32], []))
      check_stack_top(fb, break_to.result)
    }
    Return -> check_stack_top(fb, fb.result)
    Call(_function_index) -> todo
    ReturnCall(_function_index) -> todo
    CallRef(_type_index) -> todo
    ReturnCallRef(_type_index) -> todo
    End -> {
      case top_label {
        LabelIf(result: [], ..) -> Ok(fb)
        LabelIf(..) -> Error("Implicit else does not produce a result")
        _ -> Ok(fb)
      }
      |> result.try(check_stack_top_exact(_, top_label.result))
      |> result.map(fn(fb) { FunctionBuilder(..fb, label_stack: bottom_labels) })
    }
    Drop ->
      case list.length(fb.value_stack) < top_label.stack_limit + 1 {
        True -> Error("Too few values on the stack")
        False ->
          Ok(FunctionBuilder(..fb, value_stack: list.drop(fb.value_stack, 1)))
      }
    Select(_maybe_types) -> todo
    RefNull(heap_type) -> pop_push(fb, [], [Ref(Nullable(heap_type))])
    RefFunc(function_index) ->
      get_function(fb, function_index)
      |> result.map(fn(fd) { ConcreteType(fd.type_index) })
      |> result.try(fn(ht) { pop_push(fb, [], [Ref(NonNull(ht))]) })
    RefIsNull -> pop_push(fb, [Ref(Nullable(AbstractAny))], [I32])
    RefAsNonNull -> {
      stack_top(fb)
      |> result.try(fn(t) {
        case t {
          Ref(Nullable(ht)) -> pop_push(fb, [t], [Ref(NonNull(ht))])
          _ ->
            Error(
              "Expected (ref null *) at depth 0 but got "
              <> value_type_to_string(t),
            )
        }
      })
    }
    RefEq ->
      pop_push(fb, [Ref(Nullable(AbstractAny)), Ref(Nullable(AbstractAny))], [
        I32,
      ])
    RefTest(_ref_type) -> todo
    RefCast(_ref_type) -> todo
    StructNew(type_index) -> {
      get_type(fb, type_index)
      |> result.try(unpack_struct_fields(_, type_index))
      |> result.try(pop_push(fb, _, [Ref(NonNull(ConcreteType(type_index)))]))
    }
    StructNewDefault(type_index) -> {
      get_type(fb, type_index)
      |> result.try(unpack_struct_fields(_, type_index))
      |> result.try(fn(fields) {
        case list.all(fields, value_type_is_defaultable) {
          True -> pop_push(fb, [], [Ref(NonNull(ConcreteType(type_index)))])
          False ->
            Error(
              "Struct type $"
              <> int.to_string(type_index)
              <> " has non-defaultable fields",
            )
        }
      })
    }
    StructGet(type_index, field_index)
    | StructGetS(type_index, field_index)
    | StructGetU(type_index, field_index) -> {
      let packed = case instr {
        StructGetS(_, _) -> True
        StructGetU(_, _) -> True
        _ -> False
      }
      get_type(fb, type_index)
      |> result.try(fn(t) {
        case t {
          Struct(fields) ->
            list_index(fields, field_index)
            |> result.replace_error(
              "Struct type $"
              <> int.to_string(type_index)
              <> " does not have field index "
              <> int.to_string(field_index),
            )
            |> result.try(fn(field) {
              case packed, field {
                False, PackedType(_, _) ->
                  Error("struct.get not applicable to packed fields")
                False, ValueType(_, ht) -> Ok(ht)
                True, PackedType(_, _) -> Ok(I32)
                True, ValueType(_, _) ->
                  Error("struct.get_[s|u] not applicable to value types")
              }
            })
          _ ->
            Error("Type $" <> int.to_string(type_index) <> " is not a struct")
        }
      })
      |> result.try(fn(t) {
        pop_push(fb, [Ref(NonNull(ConcreteType(type_index)))], [t])
      })
    }
    StructSet(type_index, field_index) ->
      get_type(fb, type_index)
      |> result.try(fn(t) {
        case t {
          Struct(fields) ->
            list_index(fields, field_index)
            |> result.replace_error(
              "Struct type $"
              <> int.to_string(type_index)
              <> " does not have field index "
              <> int.to_string(field_index),
            )
            |> result.try(fn(field) {
              case field {
                PackedType(Immutable, _) | ValueType(Immutable, _) ->
                  Error("struct.set on immutable field")
                PackedType(Mutable, _) -> Ok(I32)
                ValueType(Mutable, vt) -> Ok(vt)
              }
            })
          _ ->
            Error("Type $" <> int.to_string(type_index) <> " is not a struct")
        }
      })
      |> result.try(fn(t) {
        pop_push(fb, [t, Ref(NonNull(ConcreteType(type_index)))], [])
      })
    LocalGet(local_index) ->
      get_local(fb, local_index)
      |> result.try(fn(t) { pop_push(fb, [], [t]) })
    LocalSet(local_index) ->
      get_local(fb, local_index)
      |> result.try(fn(t) { pop_push(fb, [t], []) })
    LocalTee(local_index) ->
      get_local(fb, local_index)
      |> result.try(fn(t) { pop_push(fb, [t], [t]) })
    GlobalGet(global_index) ->
      get_global(fb, global_index)
      |> result.try(fn(t) { pop_push(fb, [], [t]) })
    GlobalSet(global_index) ->
      get_global(fb, global_index)
      |> result.try(fn(t) { pop_push(fb, [t], []) })
    I32Const(_) -> pop_push(fb, [], [I32])
    I64Const(_) -> pop_push(fb, [], [I64])
    F32Const(_) -> pop_push(fb, [], [F32])
    F64Const(_) -> pop_push(fb, [], [F64])
    I32EqZ -> pop_push(fb, [I32], [I32])
    I32Eq
    | I32NE
    | I32LtS
    | I32LtU
    | I32GtS
    | I32GtU
    | I32LeS
    | I32LeU
    | I32GeS
    | I32GeU -> pop_push(fb, [I32, I32], [I32])
    I64EqZ -> pop_push(fb, [I64], [I32])
    I64Eq
    | I64NE
    | I64LtS
    | I64LtU
    | I64GtS
    | I64GtU
    | I64LeS
    | I64LeU
    | I64GeS
    | I64GeU -> pop_push(fb, [I64, I64], [I32])
    F32Eq | F32NE | F32Lt | F32Gt | F32Le | F32Ge ->
      pop_push(fb, [F32, F32], [I32])
    F64Eq | F64NE | F64Lt | F64Gt | F64Le | F64Ge ->
      pop_push(fb, [F64, F64], [I32])
    I32CntLZ | I32CntTZ | I32PopCnt -> pop_push(fb, [I32], [I32])
    I32Add
    | I32Sub
    | I32Mul
    | I32DivS
    | I32DivU
    | I32RemS
    | I32RemU
    | I32And
    | I32Or
    | I32Xor
    | I32ShL
    | I32ShRS
    | I32ShLU
    | I32RotL
    | I32RotR -> pop_push(fb, [I32, I32], [I32])
    I64CntLZ | I64CntTZ | I64PopCnt -> pop_push(fb, [I64], [I64])
    I64Add
    | I64Sub
    | I64Mul
    | I64DivS
    | I64DivU
    | I64RemS
    | I64RemU
    | I64And
    | I64Or
    | I64Xor
    | I64ShL
    | I64ShRS
    | I64ShLU
    | I64RotL
    | I64RotR -> pop_push(fb, [I64, I64], [I64])
    F32Abs | F32Neg | F32Ceil | F32Floor | F32Trunc | F32Nearest | F32Sqrt ->
      pop_push(fb, [F32], [F32])
    F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max | F32CopySign ->
      pop_push(fb, [F32, F32], [F32])
    F64Abs | F64Neg | F64Ceil | F64Floor | F64Trunc | F64Nearest | F64Sqrt ->
      pop_push(fb, [F64], [F64])
    F64Add | F64Sub | F64Mul | F64Div | F64Min | F64Max | F64CopySign ->
      pop_push(fb, [F64, F64], [F64])
  }
  |> result.map(fn(fb) { FunctionBuilder(..fb, code: [instr, ..fb.code]) })
}

fn get_local(fb: FunctionBuilder, local_index: Int) -> Result(ValueType, String) {
  let n_params = list.length(fb.params)
  // params are considered locals too
  case n_params > local_index {
    True -> list_index(fb.params, local_index)
    False -> {
      // locals are indexed in reverse
      let local_index =
        { list.length(fb.locals) - 1 } - { local_index - n_params }
      case local_index >= 0 {
        True -> list_index(fb.locals, local_index)
        False -> Error(Nil)
      }
    }
  }
  |> result.replace_error(
    "Local index " <> int.to_string(local_index) <> " does not exist",
  )
}

fn pop_push(fb: FunctionBuilder, pop, push) {
  check_stack_top(fb, pop)
  |> result.map(fn(fb) {
    FunctionBuilder(
      ..fb,
      value_stack: list.append(
        push,
        list.drop(fb.value_stack, list.length(pop)),
      ),
    )
  })
}

fn check_stack_top_exact(
  fb: FunctionBuilder,
  expected: List(ValueType),
) -> Result(FunctionBuilder, String) {
  use stack <- result.try(available_stack(fb))
  case list.length(stack) > list.length(expected) {
    True -> Error("Too many values on the stack")
    False -> check_stack_top_loop(fb, stack, expected, 0)
  }
  |> result.replace(fb)
}

fn check_stack_top(
  fb: FunctionBuilder,
  expected: List(ValueType),
) -> Result(FunctionBuilder, String) {
  use stack <- result.try(available_stack(fb))
  check_stack_top_loop(fb, stack, expected, 0)
  |> result.replace(fb)
}

fn check_stack_top_loop(
  fb: FunctionBuilder,
  stack: List(ValueType),
  expected: List(ValueType),
  depth: Int,
) -> Result(Nil, String) {
  case expected, stack {
    [exp, ..exp_rest], [act, ..act_rest] -> {
      case value_type_subtype_of(fb, is: act, of: exp) {
        True -> check_stack_top_loop(fb, act_rest, exp_rest, depth + 1)
        False ->
          Error(
            "Expected "
            <> value_type_to_string(exp)
            <> " at depth "
            <> int.to_string(depth)
            <> " but got "
            <> value_type_to_string(act),
          )
      }
    }
    [_exp, ..], [] -> Error("Too few values on the stack")
    [], _ -> Ok(Nil)
  }
}

fn value_type_subtype_of(
  fb: FunctionBuilder,
  is sub: ValueType,
  of sup: ValueType,
) -> Bool {
  case sub, sup {
    Ref(NonNull(sub_ht)), Ref(Nullable(sup_ht))
    | Ref(NonNull(sub_ht)), Ref(NonNull(sup_ht))
    | Ref(Nullable(sub_ht)), Ref(Nullable(sup_ht))
    -> heap_type_subtype_of(fb, sub_ht, sup_ht)
    Ref(Nullable(_)), Ref(NonNull(_)) -> False
    _, _ -> sub == sup
  }
}

fn heap_type_subtype_of(
  fb: FunctionBuilder,
  is sub: HeapType,
  of sup: HeapType,
) -> Bool {
  // TODO: hierarchies of concrete typyes
  case sub, sup {
    _, _ if sub == sup -> True
    _, AbstractAny -> True
    ConcreteType(i), AbstractArray ->
      get_type(fb, i)
      |> result.map(is_array)
      |> result.unwrap(False)
    ConcreteType(i), AbstractEq ->
      get_type(fb, i)
      |> result.replace(True)
      |> result.unwrap(False)
    AbstractArray, AbstractEq
    | AbstractFunc, AbstractEq
    | AbstractI31, AbstractEq
    | AbstractStruct, AbstractEq
    | AbstractNone, AbstractEq
    -> True
    ConcreteType(i), AbstractFunc ->
      get_type(fb, i)
      |> result.map(is_func)
      |> result.unwrap(False)
    ConcreteType(i), AbstractStruct ->
      get_type(fb, i)
      |> result.map(is_struct)
      |> result.unwrap(False)
    AbstractNoExtern, AbstractExtern -> True
    AbstractNoFunc, AbstractFunc -> True
    AbstractNoFunc, ConcreteType(i) ->
      get_type(fb, i)
      |> result.map(is_func)
      |> result.unwrap(False)
    AbstractNone, AbstractArray
    | AbstractNone, AbstractFunc
    | AbstractNone, AbstractStruct
    -> True
    AbstractNone, ConcreteType(i) ->
      get_type(fb, i)
      |> result.replace(True)
      |> result.unwrap(False)
    _, _ -> False
  }
}

fn is_array(t: CompositeType) -> Bool {
  case t {
    Array(_) -> True
    _ -> False
  }
}

fn is_struct(t: CompositeType) -> Bool {
  case t {
    Struct(_) -> True
    _ -> False
  }
}

fn is_func(t: CompositeType) -> Bool {
  case t {
    Func(_, _) -> True
    _ -> False
  }
}

fn stack_top(fb: FunctionBuilder) -> Result(ValueType, String) {
  use stack <- result.try(available_stack(fb))
  case stack {
    [] -> Error("Too few values on the stack")
    [t, ..] -> Ok(t)
  }
}

fn top_label(fb: FunctionBuilder) -> Result(Label, String) {
  case fb.label_stack {
    [top_label, ..] -> Ok(top_label)
    [] -> Error("Label stack is empty")
  }
}

fn available_stack(fb: FunctionBuilder) -> Result(List(ValueType), String) {
  use top_label <- result.map(top_label(fb))
  list.take(fb.value_stack, list.length(fb.value_stack) - top_label.stack_limit)
}

fn unpack_struct_fields(
  ct: CompositeType,
  type_index: Int,
) -> Result(List(ValueType), String) {
  case ct {
    Struct(fields) -> Ok(list.map(fields, unpack_packed_field) |> list.reverse)
    _ -> Error("Type $" <> int.to_string(type_index) <> " is not a struct")
  }
}

fn unpack_packed_field(ft: FieldType) -> ValueType {
  case ft {
    PackedType(_, _) -> I32
    ValueType(_, t) -> t
  }
}

fn value_type_is_defaultable(vt: ValueType) -> Bool {
  case vt {
    F32 -> True
    F64 -> True
    I32 -> True
    I64 -> True
    Ref(Nullable(_)) -> True
    Ref(NonNull(_)) -> False
    V128 -> True
  }
}

fn value_type_to_string(t: ValueType) {
  case t {
    I32 -> "i32"
    I64 -> "i64"
    F32 -> "f32"
    F64 -> "f64"
    V128 -> "v128"
    Ref(Nullable(ht)) -> "(ref null " <> heap_type_to_string(ht) <> ")"
    Ref(NonNull(ht)) -> "(ref " <> heap_type_to_string(ht) <> ")"
  }
}

fn heap_type_to_string(t: HeapType) {
  case t {
    AbstractAny -> "any"
    AbstractArray -> "array"
    AbstractEq -> "eq"
    AbstractExtern -> "extern"
    AbstractFunc -> "func"
    AbstractI31 -> "i32"
    AbstractNoExtern -> "noextern"
    AbstractNoFunc -> "nofunc"
    AbstractNone -> "none"
    AbstractStruct -> "struct"
    ConcreteType(idx) -> "$" <> int.to_string(idx)
  }
}

/// Add a local to the `FunctionBuilder`.
///
/// Can be done any time before the local is used.
pub fn add_local(
  fb: FunctionBuilder,
  t: ValueType,
) -> Result(#(FunctionBuilder, Int), String) {
  Ok(#(
    FunctionBuilder(
      ..fb,
      locals: [t, ..fb.locals],
      next_local_index: fb.next_local_index + 1,
    ),
    fb.next_local_index,
  ))
}

/// Complete the `FunctionBuilder` and replace the corresponding placeholder in
/// `ModuleBuilder` by the validated function code.
pub fn finalize_function(
  mb: ModuleBuilder,
  fb: FunctionBuilder,
) -> Result(ModuleBuilder, String) {
  case fb.label_stack {
    [] ->
      list_replace(
        mb.functions,
        fb.function_index,
        FunctionImplementation(
          fb.type_index,
          list.reverse(fb.locals),
          list.reverse(fb.code),
        ),
      )
      |> result.replace_error(
        "No function at index " <> int.to_string(fb.function_index),
      )
      |> result.map(fn(functions) { ModuleBuilder(..mb, functions:) })
    _ -> Error("Function incomplete")
  }
}

// --------------------------------------------------------------------------- 
// WebAssembly binary encoding
// --------------------------------------------------------------------------- 

/// Convert the WebAssembly to binary and output to file.
///
/// Assumes the WebAssembly module is valid.
pub fn emit_module(mb: ModuleBuilder) -> Result(Nil, simplifile.FileError) {
  let header = <<0x00, 0x61, 0x73, 0x6d>>
  let version = <<0x01, 0x00, 0x00, 0x00>>
  let _ = simplifile.write_bits("out.wasm", <<header:bits, version:bits>>)

  // emit types
  // TODO: recursive type groups (get rid of flatten)!
  let types = list.reverse(mb.types) |> list.flatten |> list.map(encode_typedef)
  use _ <- result.try(simplifile.append_bits(
    "out.wasm",
    encode_section(section_type, encode_vector(types)),
  ))

  // emit function type declarations
  let func_types =
    list.reverse(mb.functions)
    |> list.map(fn(func) { leb128_encode_unsigned(func.type_index) })
  use _ <- result.try(simplifile.append_bits(
    "out.wasm",
    encode_section(section_func, encode_vector(func_types)),
  ))

  // emit function code
  let func_codes =
    list.reverse(mb.functions)
    |> list.filter_map(fn(func) {
      case func {
        FunctionImplementation(_type_index, locals, code) ->
          Ok(encode_function_code(locals, code))
        FunctionImport(_, _) -> Error(Nil)
        FunctionMissing(_) -> Error(Nil)
      }
    })
  use _ <- result.try(simplifile.append_bits(
    "out.wasm",
    encode_section(section_code, encode_vector(func_codes)),
  ))

  Ok(Nil)
}

fn encode_section(section: Int, data: BitArray) {
  bit_array.append(<<section:size(8)>>, prepend_byte_size(data))
}

fn prepend_byte_size(data: BitArray) -> BitArray {
  bit_array.append(leb128_encode_unsigned(bit_array.byte_size(data)), data)
}

fn encode_vector(data: List(BitArray)) -> BitArray {
  bit_array.append(
    leb128_encode_unsigned(list.length(data)),
    list.fold(data, <<>>, bit_array.append),
  )
}

fn leb128_encode_unsigned(u: Int) -> BitArray {
  case gleb128.encode_unsigned(u) {
    Ok(val) -> val
    Error(msg) -> panic as msg
  }
}

fn encode_value_type(t: ValueType) -> BitArray {
  case t {
    F32 -> code_type_i32
    F64 -> code_type_i64
    I32 -> code_type_f32
    I64 -> code_type_f64
    Ref(NonNull(heap_type)) ->
      bit_array.append(code_ref_non_null, encode_heap_type(heap_type))
    Ref(Nullable(heap_type)) ->
      bit_array.append(code_ref_nullable, encode_heap_type(heap_type))
    V128 -> code_type_v128
  }
}

fn encode_heap_type(h: HeapType) {
  case h {
    AbstractAny -> code_type_abs_any
    AbstractArray -> code_type_abs_array
    AbstractEq -> code_type_abs_eq
    AbstractExtern -> code_type_abs_extern
    AbstractFunc -> code_type_abs_func
    AbstractI31 -> code_type_abs_i31
    AbstractNoExtern -> code_type_abs_noextern
    AbstractNoFunc -> code_type_abs_nofunc
    AbstractNone -> code_type_abs_none
    AbstractStruct -> code_type_abs_struct
    ConcreteType(index) -> leb128_encode_unsigned(index)
  }
}

fn encode_field_types(fs: List(FieldType)) -> BitArray {
  list.map(fs, encode_field_type)
  |> encode_vector
}

fn encode_field_type(f: FieldType) -> BitArray {
  let valtype = case f {
    PackedType(_, I8) -> code_type_i8
    PackedType(_, I16) -> code_type_i16
    ValueType(_, value) -> encode_value_type(value)
  }
  let mut = case f.mutable {
    Mutable -> code_var
    Immutable -> code_const
  }
  bit_array.append(valtype, mut)
}

fn encode_result_type(rs: List(ValueType)) -> BitArray {
  list.map(rs, encode_value_type)
  |> encode_vector
}

fn encode_typedef(t: CompositeType) -> BitArray {
  case t {
    Array(element) -> [code_type_array, encode_field_type(element)]
    Func(params, results) -> [
      code_type_func,
      encode_result_type(params),
      encode_result_type(results),
    ]
    Struct(fields) -> [code_type_struct, encode_field_types(fields)]
  }
  |> bit_array.concat
}

fn encode_function_code(
  locals: List(ValueType),
  code: List(Instruction),
) -> BitArray {
  [encode_result_type(locals), ..list.map(code, encode_instruction)]
  |> bit_array.concat
  |> prepend_byte_size
}

fn encode_instruction(instr: Instruction) -> BitArray {
  case instr {
    // Control instructions
    Unreachable -> code_instr_unreachable
    Nop -> code_instr_nop
    Block(block_type) ->
      bit_array.concat([code_instr_block, encode_block_type(block_type)])
    Loop(block_type) ->
      bit_array.concat([code_instr_loop, encode_block_type(block_type)])
    If(block_type) ->
      bit_array.concat([code_instr_if, encode_block_type(block_type)])
    Else -> code_instr_else
    Break(label_index) ->
      bit_array.concat([code_instr_break, leb128_encode_unsigned(label_index)])
    BreakIf(label_index) ->
      bit_array.concat([
        code_instr_break_if,
        leb128_encode_unsigned(label_index),
      ])
    Return -> code_instr_return
    Call(function_index) ->
      bit_array.concat([code_instr_call, leb128_encode_unsigned(function_index)])
    // TODO: CallIndirect
    ReturnCall(function_index) ->
      bit_array.concat([
        code_instr_return_call,
        leb128_encode_unsigned(function_index),
      ])
    // TODO: ReturnCallIndirect
    CallRef(type_index) ->
      bit_array.concat([code_instr_call_ref, leb128_encode_unsigned(type_index)])
    ReturnCallRef(type_index) ->
      bit_array.concat([
        code_instr_return_call_ref,
        leb128_encode_unsigned(type_index),
      ])
    // TODO: BreakOn*
    End -> code_instr_end
    //
    // Reference instructions
    RefNull(heap_type) ->
      bit_array.concat([code_instr_ref_null, encode_heap_type(heap_type)])
    RefFunc(function_index) ->
      bit_array.concat([
        code_instr_ref_func,
        leb128_encode_unsigned(function_index),
      ])
    RefIsNull -> code_instr_ref_is_null
    RefAsNonNull -> code_instr_ref_as_non_null
    RefEq -> code_instr_ref_eq
    RefTest(NonNull(heap_type)) ->
      bit_array.concat([
        code_instr_ref_test_nonnull,
        encode_heap_type(heap_type),
      ])
    RefTest(Nullable(heap_type)) ->
      bit_array.concat([
        code_instr_ref_test_nullable,
        encode_heap_type(heap_type),
      ])
    RefCast(NonNull(heap_type)) ->
      bit_array.concat([
        code_instr_ref_cast_nonnull,
        encode_heap_type(heap_type),
      ])
    RefCast(Nullable(heap_type)) ->
      bit_array.concat([
        code_instr_ref_cast_nullable,
        encode_heap_type(heap_type),
      ])
    StructNew(type_index) ->
      bit_array.concat([
        code_instr_struct_new,
        leb128_encode_unsigned(type_index),
      ])
    StructNewDefault(type_index) ->
      bit_array.concat([
        code_instr_struct_new_default,
        leb128_encode_unsigned(type_index),
      ])
    StructGet(type_index, field_index) ->
      bit_array.concat([
        code_instr_struct_get,
        leb128_encode_unsigned(type_index),
        leb128_encode_unsigned(field_index),
      ])
    StructGetS(type_index, field_index) ->
      bit_array.concat([
        code_instr_struct_get_s,
        leb128_encode_unsigned(type_index),
        leb128_encode_unsigned(field_index),
      ])
    StructGetU(type_index, field_index) ->
      bit_array.concat([
        code_instr_struct_get_u,
        leb128_encode_unsigned(type_index),
        leb128_encode_unsigned(field_index),
      ])
    // TODO: struct.get_s / struct.get_u
    StructSet(type_index, field_index) ->
      bit_array.concat([
        code_instr_struct_set,
        leb128_encode_unsigned(type_index),
        leb128_encode_unsigned(field_index),
      ])
    // TODO: remaining reference instructions
    //
    // Parametric instructions
    Drop -> code_instr_drop
    Select([]) -> code_instr_select
    Select(types) ->
      bit_array.concat([
        code_instr_select_typed,
        list.map(types, encode_value_type) |> encode_vector,
      ])
    //
    // Variable instructions
    LocalGet(local_index) ->
      bit_array.concat([
        code_instr_local_get,
        leb128_encode_unsigned(local_index),
      ])
    LocalSet(local_index) ->
      bit_array.concat([
        code_instr_local_set,
        leb128_encode_unsigned(local_index),
      ])
    LocalTee(local_index) ->
      bit_array.concat([
        code_instr_local_tee,
        leb128_encode_unsigned(local_index),
      ])
    GlobalGet(global_index) ->
      bit_array.concat([
        code_instr_global_get,
        leb128_encode_unsigned(global_index),
      ])
    GlobalSet(global_index) ->
      bit_array.concat([
        code_instr_global_set,
        leb128_encode_unsigned(global_index),
      ])
    //
    // Table instructions
    // TODO: table instructions
    //
    // Memory instructions
    // TODO: memory instructions
    //
    // Numeric instructions
    I32Const(value) ->
      bit_array.concat([code_instr_i32_const, gleb128.encode_signed(value)])
    I64Const(value) ->
      bit_array.concat([code_instr_i64_const, gleb128.encode_signed(value)])
    F32Const(_value) -> todo
    F64Const(_value) -> todo
    I32EqZ -> code_instr_i32_eq_z
    I32Eq -> code_instr_i32_eq
    I32NE -> code_instr_i32_ne
    I32LtS -> code_instr_i32_lt_s
    I32LtU -> code_instr_i32_lt_u
    I32GtS -> code_instr_i32_gt_s
    I32GtU -> code_instr_i32_gt_u
    I32LeS -> code_instr_i32_le_s
    I32LeU -> code_instr_i32_le_u
    I32GeS -> code_instr_i32_ge_s
    I32GeU -> code_instr_i32_ge_u
    I64EqZ -> code_instr_i64_eq_z
    I64Eq -> code_instr_i64_eq
    I64NE -> code_instr_i64_ne
    I64LtS -> code_instr_i64_lt_s
    I64LtU -> code_instr_i64_lt_u
    I64GtS -> code_instr_i64_gt_s
    I64GtU -> code_instr_i64_gt_u
    I64LeS -> code_instr_i64_le_s
    I64LeU -> code_instr_i64_le_u
    I64GeS -> code_instr_i64_ge_s
    I64GeU -> code_instr_i64_ge_u
    F32Eq -> code_instr_f32_eq
    F32NE -> code_instr_f32_ne
    F32Lt -> code_instr_f32_lt
    F32Gt -> code_instr_f32_gt
    F32Le -> code_instr_f32_le
    F32Ge -> code_instr_f32_ge
    F64Eq -> code_instr_f64_eq
    F64NE -> code_instr_f64_ne
    F64Lt -> code_instr_f64_lt
    F64Gt -> code_instr_f64_gt
    F64Le -> code_instr_f64_le
    F64Ge -> code_instr_f64_ge
    I32CntLZ -> code_instr_i32_cnt_lz
    I32CntTZ -> code_instr_i32_cnt_tz
    I32PopCnt -> code_instr_i32_pop_cnt
    I32Add -> code_instr_i32_add
    I32Sub -> code_instr_i32_sub
    I32Mul -> code_instr_i32_mul
    I32DivS -> code_instr_i32_div_s
    I32DivU -> code_instr_i32_div_u
    I32RemS -> code_instr_i32_rem_s
    I32RemU -> code_instr_i32_rem_u
    I32And -> code_instr_i32_and
    I32Or -> code_instr_i32_or
    I32Xor -> code_instr_i32_xor
    I32ShL -> code_instr_i32_sh_l
    I32ShRS -> code_instr_i32_sh_r_s
    I32ShLU -> code_instr_i32_sh_r_u
    I32RotL -> code_instr_i32_rot_l
    I32RotR -> code_instr_i32_rot_r
    I64CntLZ -> code_instr_i64_cnt_lz
    I64CntTZ -> code_instr_i64_cnt_tz
    I64PopCnt -> code_instr_i64_pop_cnt
    I64Add -> code_instr_i64_add
    I64Sub -> code_instr_i64_sub
    I64Mul -> code_instr_i64_mul
    I64DivS -> code_instr_i64_div_s
    I64DivU -> code_instr_i64_div_u
    I64RemS -> code_instr_i64_rem_s
    I64RemU -> code_instr_i64_rem_u
    I64And -> code_instr_i64_and
    I64Or -> code_instr_i64_or
    I64Xor -> code_instr_i64_xor
    I64ShL -> code_instr_i64_sh_l
    I64ShRS -> code_instr_i64_sh_r_s
    I64ShLU -> code_instr_i64_sh_r_u
    I64RotL -> code_instr_i64_rot_l
    I64RotR -> code_instr_i64_rot_r
    F32Abs -> code_instr_f32_abs
    F32Neg -> code_instr_f32_neg
    F32Ceil -> code_instr_f32_ceil
    F32Floor -> code_instr_f32_floor
    F32Trunc -> code_instr_f32_trunc
    F32Nearest -> code_instr_f32_nearest
    F32Sqrt -> code_instr_f32_sqrt
    F32Add -> code_instr_f32_add
    F32Sub -> code_instr_f32_sub
    F32Mul -> code_instr_f32_mul
    F32Div -> code_instr_f32_div
    F32Min -> code_instr_f32_min
    F32Max -> code_instr_f32_max
    F32CopySign -> code_instr_f32_copy_sign
    F64Abs -> code_instr_f64_abs
    F64Neg -> code_instr_f64_neg
    F64Ceil -> code_instr_f64_ceil
    F64Floor -> code_instr_f64_floor
    F64Trunc -> code_instr_f64_trunc
    F64Nearest -> code_instr_f64_nearest
    F64Sqrt -> code_instr_f64_sqrt
    F64Add -> code_instr_f64_add
    F64Sub -> code_instr_f64_sub
    F64Mul -> code_instr_f64_mul
    F64Div -> code_instr_f64_div
    F64Min -> code_instr_f64_min
    F64Max -> code_instr_f64_max
    F64CopySign -> code_instr_f64_copy_sign
    // TODO: conversions / truncations / etc.
    //
    // Vector instructions
    // TODO: vector instructions
  }
}

fn encode_block_type(t: BlockType) -> BitArray {
  case t {
    BlockEmpty -> code_type_empty
    BlockValue(vt) -> encode_value_type(vt)
  }
}

// --------------------------------------------------------------------------- 
// WebAssembly binary codes
// --------------------------------------------------------------------------- 

const section_custom = 0

const section_type = 1

const section_import = 2

const section_func = 3

const section_table = 4

const section_memory = 5

const section_global = 6

const section_export = 7

const section_start = 8

const section_element = 9

const section_code = 10

const section_data = 11

const code_type_i32 = <<0x7f>>

const code_type_i64 = <<0x7e>>

const code_type_f32 = <<0x7d>>

const code_type_f64 = <<0x7c>>

const code_type_v128 = <<0x7b>>

const code_type_i16 = <<0x77>>

const code_type_i8 = <<0x78>>

const code_type_abs_nofunc = <<0x73>>

const code_type_abs_noextern = <<0x72>>

const code_type_abs_none = <<0x71>>

const code_type_abs_func = <<0x70>>

const code_type_abs_extern = <<0x6f>>

const code_type_abs_any = <<0x6e>>

const code_type_abs_eq = <<0x6d>>

const code_type_abs_i31 = <<0x6c>>

const code_type_abs_struct = <<0x6b>>

const code_type_abs_array = <<0x6a>>

const code_ref_non_null = <<0x64>>

const code_ref_nullable = <<0x63>>

const code_type_func = <<0x60>>

const code_type_struct = <<0x5f>>

const code_type_array = <<0x5e>>

const code_type_rec = <<0x4e>>

const code_type_empty = <<0x40>>

const code_const = <<0x00>>

const code_var = <<0x01>>

const code_instr_unreachable = <<0x00>>

const code_instr_nop = <<0x01>>

const code_instr_block = <<0x02>>

const code_instr_loop = <<0x03>>

const code_instr_if = <<0x04>>

const code_instr_else = <<0x05>>

const code_instr_end = <<0x0b>>

const code_instr_break = <<0x0c>>

const code_instr_break_if = <<0x0d>>

const code_instr_break_table = <<0x0e>>

const code_instr_return = <<0x0f>>

const code_instr_call = <<0x10>>

const code_instr_call_indirect = <<0x11>>

const code_instr_return_call = <<0x12>>

const code_instr_return_call_indirect = <<0x13>>

const code_instr_call_ref = <<0x14>>

const code_instr_return_call_ref = <<0x15>>

const code_instr_drop = <<0x1a>>

const code_instr_select = <<0x1b>>

const code_instr_select_typed = <<0x1c>>

const code_instr_local_get = <<0x20>>

const code_instr_local_set = <<0x21>>

const code_instr_local_tee = <<0x22>>

const code_instr_global_get = <<0x23>>

const code_instr_global_set = <<0x24>>

const code_instr_i32_const = <<0x41>>

const code_instr_i64_const = <<0x42>>

const code_instr_f32_const = <<0x43>>

const code_instr_f64_const = <<0x44>>

const code_instr_i32_eq_z = <<0x45>>

const code_instr_i32_eq = <<0x46>>

const code_instr_i32_ne = <<0x47>>

const code_instr_i32_lt_s = <<0x48>>

const code_instr_i32_lt_u = <<0x49>>

const code_instr_i32_gt_s = <<0x4a>>

const code_instr_i32_gt_u = <<0x4b>>

const code_instr_i32_le_s = <<0x4c>>

const code_instr_i32_le_u = <<0x4d>>

const code_instr_i32_ge_s = <<0x4e>>

const code_instr_i32_ge_u = <<0x4f>>

const code_instr_i64_eq_z = <<0x50>>

const code_instr_i64_eq = <<0x51>>

const code_instr_i64_ne = <<0x52>>

const code_instr_i64_lt_s = <<0x53>>

const code_instr_i64_lt_u = <<0x54>>

const code_instr_i64_gt_s = <<0x55>>

const code_instr_i64_gt_u = <<0x56>>

const code_instr_i64_le_s = <<0x57>>

const code_instr_i64_le_u = <<0x58>>

const code_instr_i64_ge_s = <<0x59>>

const code_instr_i64_ge_u = <<0x5a>>

const code_instr_f32_eq = <<0x5b>>

const code_instr_f32_ne = <<0x5c>>

const code_instr_f32_lt = <<0x5d>>

const code_instr_f32_gt = <<0x5e>>

const code_instr_f32_le = <<0x5f>>

const code_instr_f32_ge = <<0x60>>

const code_instr_f64_eq = <<0x61>>

const code_instr_f64_ne = <<0x62>>

const code_instr_f64_lt = <<0x63>>

const code_instr_f64_gt = <<0x64>>

const code_instr_f64_le = <<0x65>>

const code_instr_f64_ge = <<0x66>>

const code_instr_i32_cnt_lz = <<0x67>>

const code_instr_i32_cnt_tz = <<0x68>>

const code_instr_i32_pop_cnt = <<0x69>>

const code_instr_i32_add = <<0x6a>>

const code_instr_i32_sub = <<0x6b>>

const code_instr_i32_mul = <<0x6c>>

const code_instr_i32_div_s = <<0x6d>>

const code_instr_i32_div_u = <<0x6e>>

const code_instr_i32_rem_s = <<0x6f>>

const code_instr_i32_rem_u = <<0x70>>

const code_instr_i32_and = <<0x71>>

const code_instr_i32_or = <<0x72>>

const code_instr_i32_xor = <<0x73>>

const code_instr_i32_sh_l = <<0x74>>

const code_instr_i32_sh_r_s = <<0x75>>

const code_instr_i32_sh_r_u = <<0x76>>

const code_instr_i32_rot_l = <<0x77>>

const code_instr_i32_rot_r = <<0x78>>

const code_instr_i64_cnt_lz = <<0x79>>

const code_instr_i64_cnt_tz = <<0x7a>>

const code_instr_i64_pop_cnt = <<0x7b>>

const code_instr_i64_add = <<0x7c>>

const code_instr_i64_sub = <<0x7d>>

const code_instr_i64_mul = <<0x7e>>

const code_instr_i64_div_s = <<0x7f>>

const code_instr_i64_div_u = <<0x80>>

const code_instr_i64_rem_s = <<0x81>>

const code_instr_i64_rem_u = <<0x82>>

const code_instr_i64_and = <<0x83>>

const code_instr_i64_or = <<0x84>>

const code_instr_i64_xor = <<0x85>>

const code_instr_i64_sh_l = <<0x85>>

const code_instr_i64_sh_r_s = <<0x86>>

const code_instr_i64_sh_r_u = <<0x87>>

const code_instr_i64_rot_l = <<0x88>>

const code_instr_i64_rot_r = <<0x8a>>

const code_instr_f32_abs = <<0x8b>>

const code_instr_f32_neg = <<0x8c>>

const code_instr_f32_ceil = <<0x8d>>

const code_instr_f32_floor = <<0x8e>>

const code_instr_f32_trunc = <<0x8f>>

const code_instr_f32_nearest = <<0x90>>

const code_instr_f32_sqrt = <<0x91>>

const code_instr_f32_add = <<0x92>>

const code_instr_f32_sub = <<0x93>>

const code_instr_f32_mul = <<0x94>>

const code_instr_f32_div = <<0x95>>

const code_instr_f32_min = <<0x96>>

const code_instr_f32_max = <<0x97>>

const code_instr_f32_copy_sign = <<0x98>>

const code_instr_f64_abs = <<0x99>>

const code_instr_f64_neg = <<0x9a>>

const code_instr_f64_ceil = <<0x9b>>

const code_instr_f64_floor = <<0x9c>>

const code_instr_f64_trunc = <<0x9d>>

const code_instr_f64_nearest = <<0x9e>>

const code_instr_f64_sqrt = <<0x9f>>

const code_instr_f64_add = <<0xa0>>

const code_instr_f64_sub = <<0xa1>>

const code_instr_f64_mul = <<0xa2>>

const code_instr_f64_div = <<0xa3>>

const code_instr_f64_min = <<0xa4>>

const code_instr_f64_max = <<0xa5>>

const code_instr_f64_copy_sign = <<0xa6>>

const code_instr_ref_null = <<0xd0>>

const code_instr_ref_is_null = <<0xd1>>

const code_instr_ref_func = <<0xd2>>

const code_instr_ref_eq = <<0xd3>>

const code_instr_ref_as_non_null = <<0xd4>>

const code_instr_struct_new = <<0xfb, 0x00>>

const code_instr_struct_new_default = <<0xfb, 0x01>>

const code_instr_struct_get = <<0xfb, 0x02>>

const code_instr_struct_get_s = <<0xfb, 0x03>>

const code_instr_struct_get_u = <<0xfb, 0x04>>

const code_instr_struct_set = <<0xfb, 0x05>>

const code_instr_ref_test_nonnull = <<0xfb, 0x14>>

const code_instr_ref_test_nullable = <<0xfb, 0x15>>

const code_instr_ref_cast_nonnull = <<0xfb, 0x16>>

const code_instr_ref_cast_nullable = <<0xfb, 0x17>>
