//! Smoke test for integer SIMD add/sub intrinsic lowering.

//@ run-pass
//@ exit-code: 0

#![feature(repr_simd, core_intrinsics)]
#![allow(internal_features, non_camel_case_types)]

use core::intrinsics::simd::{simd_add, simd_sub};

#[repr(simd)]
struct i32x4([i32; 4]);

#[repr(simd)]
struct u32x4([u32; 4]);

// CHECK-LABEL: main
// CHECK: __attribute__((vector_size(16)))
// CHECK: +
// CHECK: -
fn main() {
    unsafe {
        let sum = simd_add(i32x4([1, 2, 3, 4]), i32x4([10, 20, 30, 40]));
        let sum_arr: [i32; 4] = core::mem::transmute(sum);
        if sum_arr != [11, 22, 33, 44] {
            std::process::exit(1);
        }

        let diff = simd_sub(i32x4([10, 20, 30, 40]), i32x4([1, 2, 3, 4]));
        let diff_arr: [i32; 4] = core::mem::transmute(diff);
        if diff_arr != [9, 18, 27, 36] {
            std::process::exit(2);
        }

        let udiff = simd_sub(u32x4([0, 1, 2, 3]), u32x4([1, 1, 3, 4]));
        let udiff_arr: [u32; 4] = core::mem::transmute(udiff);
        if udiff_arr != [u32::MAX, 0, u32::MAX, u32::MAX] {
            std::process::exit(3);
        }
    }
}
