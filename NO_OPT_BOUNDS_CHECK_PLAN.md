# rustc_codegen_c no-opt bounds-check support plan

This document captures a step-by-step implementation plan to support safe array indexing
(`arr[i]`) in `rustc_codegen_c` when optimization is disabled (no `-O`), following the
same high-level strategy used by `rustc_codegen_cranelift`.

## Goal

1. `./y test` (default, no `-O`) can compile and run safe indexing paths.
2. Bounds checks are lowered through MIR `TerminatorKind::Assert`, not hidden in `gep`.
3. Panic path calls lang items (especially `panic_bounds_check`) and reaches C runtime helpers.

## Key design principles

- Keep `PlaceElem::Index/ConstantIndex` for address calculation only.
- Handle bounds checks in `TerminatorKind::Assert` lowering.
- Use lang-item panic functions for semantics; C runtime helpers are implementation detail.

## Step-by-step plan

### Step 1: Provide minimal lang-item panic stubs [Done]

Files:
- `tests/auxiliary/mini_core.rs`
- `crates/rustc_codegen_c/src/helper.h`

Actions:
- Add `#[lang = "panic_bounds_check"]` in `mini_core`.
- Add `#[lang = "panic_location"]` if required by the current compiler path.
- In `helper.h`, add a helper like `__rust_panic_bounds_check(size_t index, size_t len)`
  that prints diagnostics and aborts.
- Make `mini_core::panic_bounds_check` call that helper via `extern "C"`.

Acceptance:
- Frontend no longer errors with `requires panic_bounds_check lang_item`.

### Step 2: Introduce real basic-block model in backend [Done]

Files:
- `crates/rustc_codegen_c/src/context.rs`
- `crates/rustc_codegen_c/src/builder.rs`

Actions:
- Replace `BasicBlock = CFunc` with a dedicated block handle/type.
- Keep per-function block table and deterministic labels.
- Let `Builder` track current active block.

Acceptance:
- Backend can create multiple blocks before emitting body.

### Step 3: Add control-flow statements to C AST [Done]

Files:
- `crates/rustc_codegen_c_ast/src/stmt.rs`
- (if needed) `crates/rustc_codegen_c_ast/src/expr.rs`

Actions:
- Add statement forms for label/goto/conditional branch.
- Suggested minimal set:
  - `Label(name)`
  - `Goto(name)`
  - `IfGoto { cond, then_label, else_label }`
- Update printer to emit valid C.

Acceptance:
- AST can represent MIR terminator-level branching directly.

### Step 4: Implement block-management builder methods [Done]

Files:
- `crates/rustc_codegen_c/src/builder.rs`

Actions:
- Implement:
  - `append_block` / `append_sibling_block`
  - `switch_to_block`
  - `llbb`
- Ensure statements are appended to current block body.

Acceptance:
- MIR traversal can switch emission target blocks safely.

### Step 5: Implement core branch terminators [Done]

Files:
- `crates/rustc_codegen_c/src/builder.rs`

Actions:
- Implement:
  - `br` -> `goto`
  - `cond_br` -> `if (...) goto ...; else goto ...;`
  - `unreachable` -> `abort()` (or `__builtin_unreachable` + abort)
  - `switch` (initially as if-else chain if easier)

Acceptance:
- No-opt MIR with `Assert` and simple branches can lower.

### Step 6: Complete bool/compare support used by asserts [Done]

Files:
- `crates/rustc_codegen_c/src/builder.rs`
- `crates/rustc_codegen_c/src/context/base_type.rs`
- `crates/rustc_codegen_c/src/context/layout_type.rs`

Actions:
- Implement missing comparison and bool-producing ops required by assert conditions,
  especially `icmp`.
- Ensure bool backend/immediate types are supported.

Acceptance:
- Assert condition values can be produced and consumed by `cond_br`.

### Step 7: Make panic call path robust [Done]

Files:
- `crates/rustc_codegen_c/src/context/pre_define.rs`
- `crates/rustc_codegen_c/src/context/misc.rs`
- `crates/rustc_codegen_c/src/builder.rs`

Actions:
- Ensure lang-item panic functions are resolvable to callable C symbols.
- Keep symbol sanitization for valid C identifiers.
- Ensure `call` handles void-return panic helpers correctly.

Acceptance:
- Panic path emits valid call statements and terminates control flow.

### Step 8: Add focused no-opt regression tests [Done]

Files:
- `tests/codegen/array_index_const.rs`
- (optional) new test files under `tests/codegen/`

Actions:
- Keep one run-pass safe-index test in no-opt mode.
- Add FileCheck assertions for:
  - emitted branch structure
  - panic helper or panic lang-item call symbol
- Add one out-of-bounds test if runtime behavior is deterministic in test environment.

Acceptance:
- `./y test` passes without `-O` for safe indexing coverage.

### Step 9: Optional polish and follow-ups

Actions:
- Replace if-chain switch lowering with real C `switch` where beneficial.
- Improve panic diagnostics formatting and helper naming.
- Expand from constant index to variable index coverage in tests.

## Suggested commit sequence

1. `mini_core + helper.h` lang-item panic stubs.
2. Basic-block model + C AST control-flow statements.
3. Branch terminators (`br`, `cond_br`, `unreachable`, `switch`).
4. `icmp`/bool support + panic call stabilization.
5. no-opt tests and cleanup.

## Out of scope for this plan

- Full Rust layout fidelity for all types.
- Complete panic/unwind ecosystem beyond what is needed for bounds-check path.
- Full intrinsic coverage unrelated to array-index asserts.

## Array typing follow-up

The no-opt bounds-check pipeline is complete, but generated C still uses byte-buffer style
stack slots for arrays (for example `uint8_t buf[N]` with casts) instead of typed array
declarations/usages. The follow-up migration is tracked here.

### Phase 1: Introduce pointer/place metadata in builder [Done]

Actions:
- Add pointer metadata table in `Builder` to track inferred pointee type.
- Record metadata on `alloca` and derived pointers from `gep`.
- Use metadata in `store` before falling back to align-based guessing.

Status:
- Implemented in `crates/rustc_codegen_c/src/builder.rs`.

### Phase 2: Type-driven GEP projection [Done]

Actions:
- Allow non-constant array/pointer indices in `gep`.
- Keep struct field indexing constant-only.
- Continue to update pointer metadata for projected pointers.

Status:
- Implemented for dynamic array/pointer indices.
- Struct field indexing remains constant-only by design.

### Phase 3: Typed stack-slot declarations [Done]

Actions:
- Replace raw byte-array declaration strategy for `alloca` with deferred typed slot materialization.
- Ensure emitted C locals use concrete aggregate element types (`int32_t arr[3]`, etc.).

Status:
- Deferred alloca declaration is implemented.
- Array declarations in indexed and non-indexed paths now emit typed locals (`int32_t _0[3]`, etc.).
- GEP-derived load/store now uses direct array lvalues where possible (`_0[i]` reads/writes).

### Phase 4: Remove remaining type-guess fallbacks [In Progress]

Actions:
- Remove align-based store-type fallback once typed place propagation is complete.
- Verify no `size_t` accidental widening in integer array write paths.

Status:
- `store` prefers propagated type info (value/pointee/pending-allocation) and only then uses align fallback.
- Added per-function shared value type cache across basic blocks to reduce type loss at branch joins.
- Added variable-index no-opt regression coverage in `tests/codegen/array_index_var.rs`.
