# Panic=Abort Semantic Alignment Plan

This document defines an implementation plan for making `rustc_codegen_c` semantically aligned with Rust's `panic=abort` behavior.

## 1. Goal

Deliver a backend contract that is explicit, testable, and stable:

- Generated programs follow Rust `panic=abort` behavior.
- No backend path requires stack unwinding machinery.
- EH-related `todo!()` paths are removed from the active call/unwind surface.
- Failures in unsupported unwind-only paths are explicit and deterministic.

## 2. Scope

In scope:

- Call/unwind lowering behavior in `crates/rustc_codegen_c/src/builder.rs`.
- Inline-asm unwind interaction in `crates/rustc_codegen_c/src/builder/asm.rs`.
- Backend and bootstrap guardrails that enforce `panic=abort`.
- Regression tests for abort-only call/cleanup behavior.

Out of scope:

- `panic=unwind` support.
- Full Itanium/MSVC EH model in generated C.
- `catch_unwind` parity under unwinding semantics.
- Cross-language exception interop.

## 3. Current State Summary

The project already forces `panic=abort` in bootstrap:

- `bootstrap/src/manifest.rs`
  - `backend_rustflags()` includes `-Cpanic=abort`
  - `rustc()` includes `-C panic=abort`

Main EH gaps still present in backend:

- `crates/rustc_codegen_c/src/builder.rs`
  - `invoke`
  - `cleanup_landing_pad`
  - `filter_landing_pad`
  - `cleanup_pad`
  - `cleanup_ret`
  - `catch_pad`
  - `catch_switch`
  - `tail_call` (call control-flow surface, not EH-only but part of the same incomplete area)
- `crates/rustc_codegen_c/src/builder/asm.rs`
  - `codegen_inline_asm` with unwind/catch funclet plumbing is unimplemented

## 4. Semantic Contract (Target)

After this plan is implemented, backend behavior must be:

1. Effective panic strategy is `abort`.
2. Rust panic never unwinds through generated C frames.
3. Any unexpectedly reached unwind-only backend path aborts immediately.
4. Normal non-unwind control-flow (including `Drop` on normal paths) remains correct.

## 5. Design Decisions

### 5.1 Abort-only EH Policy

Treat unwinding as unsupported runtime behavior for this backend. For APIs that exist in `rustc_codegen_ssa` trait surfaces but represent unwind semantics, implement a deterministic abort path instead of leaving `todo!()`.

### 5.2 Explicit Guardrails

Keep `-Cpanic=abort` forced in bootstrap, and add backend-side validation against effective session panic strategy to prevent silent drift.

### 5.3 No libunwind Dependency

Do not integrate `libunwind` for this milestone. It does not by itself provide Rust language-level EH semantics and increases complexity with low short-term value for abort-only mode.

## 6. Implementation Plan

### Phase A: Enforce Backend Contract (Done)

Tasks:

1. Add a backend-side assertion/check that effective panic strategy is `Abort`.
2. Keep bootstrap panic forcing as-is.
3. Add a clear diagnostic message for unsupported unwind mode if the check fails.

Suggested locations:

- `crates/rustc_codegen_c/src/base.rs` (or early crate codegen entrypoint)
- Optional: `bootstrap/src/cargo.rs` cleanup of conflicting panic flags before appending backend flags

Acceptance:

- Backend fails fast with a clear message if not effectively in abort mode.

Completion:

- Added backend panic strategy hard check (`panic=abort` required).
- Normalized bootstrap rustflags by stripping pre-existing panic flags before backend injection.
- Added/updated unit tests for panic strategy validation and rustflag normalization behavior.

### Phase B: Complete Abort-only Call/Unwind Lowering (Done)

Tasks in `crates/rustc_codegen_c/src/builder.rs`:

1. `invoke`:
   - Lower as plain `call`.
   - Continue into `then` block.
   - Treat unwind edge as unreachable in abort-only mode.
2. `cleanup_landing_pad` / `filter_landing_pad` / `cleanup_pad` / `cleanup_ret` / `catch_pad` / `catch_switch`:
   - Implement explicit abort-unreachable behavior instead of `todo!()`.
   - Return placeholder values only where type signatures require returning a value after emitting abort/unreachable.
3. `resume`:
   - Keep/confirm abort-unreachable semantics.
4. `tail_call`:
   - Implement minimal non-unwind behavior or explicit fatal diagnostic (no `todo!()`).

Acceptance:

- No `todo!()` remains in the EH/call-control methods above.
- Existing callsites compile and run in abort-only mode.

Completion:

- Replaced `invoke` with abort-only lowering (`call` + branch to `then`).
- Implemented abort-unreachable behavior for cleanup/catch EH methods:
  `cleanup_landing_pad`, `filter_landing_pad`, `cleanup_pad`, `cleanup_ret`, `catch_pad`,
  `catch_switch`.
- Implemented `tail_call` by reusing call lowering and emitting return according to `PassMode`.

### Phase C: Inline ASM Unwind Handling

Tasks in `crates/rustc_codegen_c/src/builder/asm.rs`:

1. Implement non-unwinding inline-asm path.
2. If asm may unwind or catch funclet is requested, emit explicit unsupported diagnostic or abort path (deterministic behavior, no `todo!()`).

Acceptance:

- Inline asm path no longer panics on unimplemented code for supported abort-only usage.

### Phase D: Tests and Regression Coverage

Add/adjust tests in `tests/codegen`:

1. Keep and run existing cleanup regression:
   - `std_refcell_cleanup_smoke.rs`
2. Add abort-only invoke/cleanup regression:
   - A case that exercises a call with a MIR cleanup edge while still succeeding in normal path.
3. Add inline-asm coverage:
   - Non-unwinding inline-asm path compiles/runs.
4. Add backend contract check coverage:
   - A harness/unit test (bootstrap or backend test) that verifies panic strategy guard behavior.

Acceptance:

- `./y test --stage run` passes.
- Added regressions fail before fix and pass after fix.

## 7. Rollout Strategy

1. Land Phase A first (contract enforcement).
2. Land Phase B in small commits by method group:
   - `invoke` + `tail_call`
   - cleanup/catch group
3. Land Phase C.
4. Land Phase D tests and finalize docs.

## 8. Risks and Mitigations

Risk: Hidden unwind edges from uncommon MIR paths.
Mitigation: explicit abort in unsupported EH methods; add targeted regression cases.

Risk: Behavior mismatch when callers pass conflicting panic flags.
Mitigation: backend-side effective panic strategy check; optional bootstrap flag normalization.

Risk: Platform-specific abort exit behavior in runtime tests.
Mitigation: prefer semantic checks over fragile signal-code-only assertions where practical.

## 9. Done Criteria

This plan is complete when:

1. Abort-only contract is enforced and documented.
2. EH/call-control `todo!()` methods listed in Section 3 are removed.
3. Test suite includes explicit abort-only cleanup/call coverage.
4. `std_refcell_cleanup_smoke` and full `./y test --stage run` pass.
