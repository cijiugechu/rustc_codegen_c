//! Smoke test for non-integer SIMD lane type lowering.

//@ run-pass
//@ exit-code: 0

#![feature(repr_simd)]
#![allow(non_camel_case_types)]

#[repr(simd)]
struct f32x4([f32; 4]);

// CHECK-LABEL: main
// CHECK: __attribute__((vector_size(16)))
fn main() {
    let v = f32x4([1.5, -2.0, 3.25, 4.0]);
    let lanes: [f32; 4] = unsafe { core::mem::transmute(v) };
    if lanes != [1.5, -2.0, 3.25, 4.0] {
        std::process::exit(1);
    }
}
