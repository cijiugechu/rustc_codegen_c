//! Smoke test for SIMD extract/insert intrinsic lowering.

//@ run-pass
//@ exit-code: 0

#![feature(repr_simd, core_intrinsics)]
#![allow(internal_features, non_camel_case_types)]

use core::intrinsics::simd::{simd_extract, simd_extract_dyn, simd_insert, simd_insert_dyn};

#[repr(simd)]
#[derive(Copy, Clone)]
struct i32x4([i32; 4]);

#[repr(simd)]
#[derive(Copy, Clone)]
struct f32x4([f32; 4]);

// CHECK-LABEL: main
// CHECK: __attribute__((vector_size(16)))
fn main() {
    unsafe {
        let v = i32x4([10, 20, 30, 40]);
        let with_const = simd_insert(v, 1, -7);
        let with_dyn = simd_insert_dyn(with_const, 3_u32, 99);
        let lanes: [i32; 4] = core::mem::transmute(with_dyn);
        if lanes != [10, -7, 30, 99] {
            std::process::exit(1);
        }

        let first: i32 = simd_extract(with_dyn, 0_u32);
        if first != 10 {
            std::process::exit(2);
        }

        let idx = 3_u32;
        let last: i32 = simd_extract_dyn(with_dyn, idx);
        if last != 99 {
            std::process::exit(3);
        }

        let fv = f32x4([1.0, 2.0, 3.0, 4.0]);
        let fv2 = simd_insert_dyn(fv, 2_u32, 9.5_f32);
        let flanes: [f32; 4] = core::mem::transmute(fv2);
        if flanes != [1.0, 2.0, 9.5, 4.0] {
            std::process::exit(4);
        }

        let fidx = 2_u32;
        let lane: f32 = simd_extract_dyn(fv2, fidx);
        if lane != 9.5 {
            std::process::exit(5);
        }
    }
}
