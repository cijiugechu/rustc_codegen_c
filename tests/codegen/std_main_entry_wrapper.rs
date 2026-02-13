//! Test std main entry wrapper generation.
//!
//! This uses the default std startup path and expects backend-generated C entry `main`.

//@ run-pass
//@ exit-code: 0

// CHECK-LABEL: int32_t main(
// CHECK: lang_start
pub fn main() {}
