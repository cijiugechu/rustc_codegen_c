# Integer `+` MVP Notes

This document tracks the current MVP state for integer addition codegen and its known semantic gaps.

## Scope

The current implementation only targets primitive integer addition:

- signed integers: `i8`, `i16`, `i32`, `i64`, `isize`
- unsigned integers: `u8`, `u16`, `u32`, `u64`, `usize`

Out of scope for this MVP:

- floating-point arithmetic
- custom `Add` implementations for user types
- other binary ops (`-`, `*`, `/`, shifts, bit ops, checked ops)

## Current Behavior

- `BuilderMethods::add` lowers integer `lhs + rhs` into a C expression `(<lhs> + <rhs>)`.
- The generated expression is materialized into a temporary local C variable and then returned/used.
- `mini_core` now defines a minimal `#[lang = "add"]` trait and primitive integer impls so `no_core` tests can type-check `+`.
- `./y rustc` now passes `-C overflow-checks=off` by default to avoid the unimplemented checked-overflow codegen path.

## Semantic Gaps (Important)

1. Overflow checks are disabled globally in bootstrap (`-C overflow-checks=off`).
2. Signed integer overflow in generated C uses plain `+`, which can invoke C UB.
3. C integer promotions may differ from Rust's exact intermediate semantics for narrow types.
4. `checked_binop` and overflow-reporting paths are still unimplemented.
5. The implementation currently assumes simple scalar integer operands and does not cover aggregate/complex value forms.
6. Non-integer `add` remains unsupported and is expected to fail/panic in codegen.

## Follow-up Checklist

- [ ] Replace plain signed `+` lowering with overflow-safe semantics compatible with Rust.
- [ ] Implement checked integer add lowering (`checked_binop`) and honor `overflow-checks=on`.
- [ ] Remove global `-C overflow-checks=off` workaround once checked paths are supported.
- [ ] Add tests for all primitive integer widths and signed/unsigned combinations.
- [ ] Add edge-case tests for overflow boundaries (`MAX + 1`, `MIN - 1` style cases).
- [ ] Extend the same pattern to other integer binops (`sub`, `mul`, shifts, bit ops).
