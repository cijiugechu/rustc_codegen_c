# rustc_codegen_c Fat Pointer Support Plan

This plan tracks incremental support for fat pointers in `rustc_codegen_c`, focusing first on
slice-like pointers (`&[u8]`, `&str`) and keeping parity with the `rustc_codegen_cranelift`
strategy: represent data pointer and metadata separately, and consume `TyAndLayout` as the source
of truth.

## Scope

- In scope (initial): `&[T]`, `&str`, and basic scalar-pair argument/operand flow.
- Out of scope (initial): trait object vtables, custom DST layouts, full unsize coercion matrix.

## Phase Plan

### Phase 1: Scalar-pair operand foundations [Done]

Files:
- `crates/rustc_codegen_c/src/builder.rs`

Actions:
- Implement `load_operand` scalar-pair loading from memory places.
- Implement `extract_value`/`insert_value` for scalar-pair values.
- Ensure pair element loads use layout-derived element backend types and offsets.

Acceptance:
- MIR paths that materialize scalar pairs no longer hit `todo!()` in these methods.

### Phase 2: Minimal ABI pair argument plumbing [Done]

Files:
- `crates/rustc_codegen_c/src/builder/abi.rs`

Actions:
- Implement `store_fn_arg` and `store_arg` for:
  - `PassMode::Direct`
  - `PassMode::Pair`
  - `PassMode::Ignore`
- Keep unsupported ABI modes explicitly rejected with clear messages.

Acceptance:
- Functions with pair ABI arguments can be lowered in simple cases.

### Phase 3: Initial slice-oriented tests [Done]

Files:
- `tests/codegen/slice_len.rs` (new)
- `tests/codegen/slice_arg_len.rs` (new)

Actions:
- Add no-core run-pass tests covering scalar-pair slice argument flow:
  - passing `&[u8]` across one function boundary
  - local copy/reuse of `&[u8]` before a call

Acceptance:
- `./y test` covers scalar-pair call flow for slices.

### Phase 4: Constants and static backing data [Done]

Files:
- `crates/rustc_codegen_c/src/context/const.rs`
- `crates/rustc_codegen_c/src/context/pre_define.rs`

Actions:
- Implement enough constant slice/string materialization to avoid null-pointer fallbacks for
  regular code paths.

Acceptance:
- `&'static str` / static byte slice usage can lower without placeholder pointer values.

### Phase 5: Extended fat pointer support [Done]

Files:
- `crates/rustc_codegen_c/src/builder.rs`
- `crates/rustc_codegen_c/src/builder/abi.rs`

Actions:
- Expand from slice pairs to broader DST/fat-pointer workflows.
- Validate `&str` scalar-pair call flow (literal and forwarded) in no-core codegen tests.

Acceptance:
- Additional fat-pointer-heavy tests pass.

## Implemented Notes

- `context/const.rs` now materializes string/byte constant pointers instead of null fallbacks.
- Pointer scalar constants now lower through global-alloc aware paths and preserve byte offsets.
- Scalar-pair pointer element lowering now uses pointer C types (not `size_t`) for pointer fields.
- Added tests:
  - `tests/codegen/str_literal_arg.rs`
  - `tests/codegen/str_literal_forward.rs`
