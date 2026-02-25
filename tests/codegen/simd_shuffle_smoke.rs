//! Smoke test for SIMD shuffle intrinsic lowering.

//@ run-pass
//@ exit-code: 0

#![feature(repr_simd, core_intrinsics)]
#![allow(internal_features, non_camel_case_types)]

use core::intrinsics::simd::simd_shuffle;

#[repr(simd)]
struct i32x4([i32; 4]);

#[repr(simd)]
struct u32x4([u32; 4]);

#[repr(simd)]
struct f32x4([f32; 4]);

const IDX_I32: u32x4 = u32x4([0, 5, 2, 7]);
const IDX_F32: u32x4 = u32x4([4, 1, 6, 3]);

// CHECK-LABEL: main
// CHECK: __attribute__((vector_size(16)))
fn main() {
    unsafe {
        let a = i32x4([1, 2, 3, 4]);
        let b = i32x4([10, 20, 30, 40]);
        let mixed: i32x4 = simd_shuffle(a, b, IDX_I32);
        let lanes: [i32; 4] = core::mem::transmute(mixed);
        if lanes != [1, 20, 3, 40] {
            std::process::exit(1);
        }

        let fa = f32x4([1.0, 2.0, 3.0, 4.0]);
        let fb = f32x4([5.0, 6.0, 7.0, 8.0]);
        let fmix: f32x4 = simd_shuffle(fa, fb, IDX_F32);
        let flanes: [f32; 4] = core::mem::transmute(fmix);
        if flanes != [5.0, 2.0, 7.0, 4.0] {
            std::process::exit(2);
        }
    }
}
