//! Ensure closure layout gets a concrete backend struct type instead of erasing to `uint8_t*`.

#[inline(never)]
fn run<F: Fn(i32) -> i32>(f: F) -> i32 {
    f(10)
}

// CHECK: struct __rcgenc_closure_{{[0-9]+}} {
// CHECK: int32_t _ZN{{[[:alnum:]_]+}}run17h{{[[:xdigit:]]+}}E(struct __rcgenc_closure_{{[0-9]+}} *_0);
// CHECK-NOT: run17h{{[[:xdigit:]]+}}E(uint8_t *_0);
pub fn main() {
    let a = 1i32;
    let b = 2i32;
    let c = 3i32;
    let f = |x: i32| x + a + b + c;
    let _ = run(f);
}
