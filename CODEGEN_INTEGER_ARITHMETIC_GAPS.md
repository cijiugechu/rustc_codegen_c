# Integer Arithmetic Codegen Gaps

This document tracks what integer arithmetic is implemented in `rustc_codegen_c` today, and what is still missing.

## Implemented Today

Primitive integer lowering currently works for:

- `add` (`+`)
- `sub` (`-`)
- `mul` (`*`)
- `udiv` (`/` for unsigned integer operands)
- `sdiv` (`/` for signed integer operands)

The implementation supports primitive integer scalar types (`i8/i16/i32/i64/isize`, `u8/u16/u32/u64/usize`) and emits C temporary locals with the corresponding operator expression.

`mini_core` currently provides enough lang items to type-check `+`, `-`, and `*` in `no_core` tests.

## Known Gaps

1. Overflow checks are still disabled globally in bootstrap (`-C overflow-checks=off`).
2. `checked_binop` and overflow-reporting codegen paths are unimplemented.
3. Signed integer arithmetic currently lowers to plain C operators, which can trigger C UB on overflow.
4. C integer promotions may not exactly match Rust semantics for narrow integer intermediates.
5. Division coverage is incomplete at test level (`udiv/sdiv` are implemented but there is no stable `no_core` regression test kept in-tree yet).
6. Integer remainder and exact division helpers are still missing (`urem`, `srem`, `exactudiv`, `exactsdiv`).
7. Bitwise and shift integer ops are still missing (`and`, `or`, `xor`, `shl`, `lshr`, `ashr`).
8. Comparison/branch-related pieces are incomplete in backend builder (`icmp`/control-flow pieces still contain `todo!()` paths).
9. Non-integer arithmetic (`fadd/fsub/fmul/fdiv/...`) is still unsupported.
10. Aggregate/complex value forms are not fully covered; current arithmetic lowering assumes scalar operands.

## Follow-up Checklist

- [ ] Implement checked integer arithmetic and remove the global `overflow-checks=off` workaround.
- [ ] Define overflow-safe semantics for signed integer ops in generated C.
- [ ] Complete integer arithmetic surface (`div/rem/exact div/shifts/bitwise`) end-to-end.
- [ ] Implement missing comparison/control-flow builder methods needed by arithmetic edge paths.
- [ ] Add stable regression tests for integer division once the no-core runtime/checking path is available.
- [ ] Add edge-case tests for overflow boundaries and division corner cases.
