//! Smoke test for SIMD compare intrinsic lowering.

//@ run-pass
//@ exit-code: 0

#![feature(repr_simd, core_intrinsics)]
#![allow(internal_features, non_camel_case_types)]

use core::intrinsics::simd::{simd_eq, simd_ge, simd_gt, simd_le, simd_lt, simd_ne};

#[repr(simd)]
#[derive(Copy, Clone)]
struct i32x4([i32; 4]);

#[repr(simd)]
#[derive(Copy, Clone)]
struct u32x4([u32; 4]);

#[repr(simd)]
#[derive(Copy, Clone)]
struct f32x4([f32; 4]);

// CHECK-LABEL: main
// CHECK: __attribute__((vector_size(16)))
// CHECK: ==
// CHECK: !=
// CHECK: <
// CHECK: <=
// CHECK: >
// CHECK: >=
fn main() {
    const T: u32 = u32::MAX;

    unsafe {
        let a = i32x4([1, -2, 5, -8]);
        let b = i32x4([1, -3, 5, 10]);

        let eq: u32x4 = simd_eq(a, b);
        let eq_arr: [u32; 4] = core::mem::transmute(eq);
        if eq_arr != [T, 0, T, 0] {
            std::process::exit(1);
        }

        let ne: u32x4 = simd_ne(a, b);
        let ne_arr: [u32; 4] = core::mem::transmute(ne);
        if ne_arr != [0, T, 0, T] {
            std::process::exit(2);
        }

        let lt: u32x4 = simd_lt(a, b);
        let lt_arr: [u32; 4] = core::mem::transmute(lt);
        if lt_arr != [0, 0, 0, T] {
            std::process::exit(3);
        }

        let le: u32x4 = simd_le(a, b);
        let le_arr: [u32; 4] = core::mem::transmute(le);
        if le_arr != [T, 0, T, T] {
            std::process::exit(4);
        }

        let gt: u32x4 = simd_gt(a, b);
        let gt_arr: [u32; 4] = core::mem::transmute(gt);
        if gt_arr != [0, T, 0, 0] {
            std::process::exit(5);
        }

        let ge: u32x4 = simd_ge(a, b);
        let ge_arr: [u32; 4] = core::mem::transmute(ge);
        if ge_arr != [T, T, T, 0] {
            std::process::exit(6);
        }

        let ua = u32x4([0, 1, 10, u32::MAX]);
        let ub = u32x4([1, 1, 3, 0x8000_0000]);

        let ult: u32x4 = simd_lt(ua, ub);
        let ult_arr: [u32; 4] = core::mem::transmute(ult);
        if ult_arr != [T, 0, 0, 0] {
            std::process::exit(7);
        }

        let ugt: u32x4 = simd_gt(ua, ub);
        let ugt_arr: [u32; 4] = core::mem::transmute(ugt);
        if ugt_arr != [0, 0, T, T] {
            std::process::exit(8);
        }

        let fa = f32x4([1.0, f32::NAN, -2.0, f32::NAN]);
        let fb = f32x4([1.0, f32::NAN, 3.0, 1.0]);

        let feq: u32x4 = simd_eq(fa, fb);
        let feq_arr: [u32; 4] = core::mem::transmute(feq);
        if feq_arr != [T, 0, 0, 0] {
            std::process::exit(9);
        }

        let fne: u32x4 = simd_ne(fa, fb);
        let fne_arr: [u32; 4] = core::mem::transmute(fne);
        if fne_arr != [0, T, T, T] {
            std::process::exit(10);
        }

        let flt: u32x4 = simd_lt(fa, fb);
        let flt_arr: [u32; 4] = core::mem::transmute(flt);
        if flt_arr != [0, 0, T, 0] {
            std::process::exit(11);
        }

        let fge: u32x4 = simd_ge(fa, fb);
        let fge_arr: [u32; 4] = core::mem::transmute(fge);
        if fge_arr != [T, 0, 0, 0] {
            std::process::exit(12);
        }
    }
}
