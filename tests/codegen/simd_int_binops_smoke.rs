//! Smoke test for integer SIMD mul/shift/bitwise intrinsic lowering.

//@ run-pass
//@ exit-code: 0

#![feature(repr_simd, core_intrinsics)]
#![allow(internal_features, non_camel_case_types)]

use core::intrinsics::simd::{
    simd_and, simd_mul, simd_or, simd_shl, simd_shr, simd_xor,
};

#[repr(simd)]
struct i32x4([i32; 4]);

#[repr(simd)]
struct u32x4([u32; 4]);

// CHECK-LABEL: main
// CHECK: __attribute__((vector_size(16)))
// CHECK: *
// CHECK: <<
// CHECK: >>
// CHECK: &
// CHECK: |
// CHECK: ^
fn main() {
    unsafe {
        let mul = simd_mul(i32x4([2, -3, 4, -5]), i32x4([6, 7, -8, -9]));
        let mul_arr: [i32; 4] = core::mem::transmute(mul);
        if mul_arr != [12, -21, -32, 45] {
            std::process::exit(1);
        }

        let shl = simd_shl(u32x4([1, 2, 3, 4]), u32x4([0, 1, 2, 3]));
        let shl_arr: [u32; 4] = core::mem::transmute(shl);
        if shl_arr != [1, 4, 12, 32] {
            std::process::exit(2);
        }

        let shr_u = simd_shr(u32x4([8, 16, 32, 64]), u32x4([1, 2, 3, 4]));
        let shr_u_arr: [u32; 4] = core::mem::transmute(shr_u);
        if shr_u_arr != [4, 4, 4, 4] {
            std::process::exit(3);
        }

        let shr_i = simd_shr(i32x4([-2, -4, -8, 16]), i32x4([1, 1, 2, 2]));
        let shr_i_arr: [i32; 4] = core::mem::transmute(shr_i);
        if shr_i_arr != [-1, -2, -2, 4] {
            std::process::exit(4);
        }

        let and_arr: [u32; 4] = core::mem::transmute(simd_and(
            u32x4([0b1010, 0b1100, 0xFFFF_0000, 0xF0F0]),
            u32x4([0b0110, 0b1010, 0x00FF_FF00, 0x0FF0]),
        ));
        if and_arr != [0b0010, 0b1000, 0x00FF_0000, 0x00F0] {
            std::process::exit(5);
        }

        let or_arr: [u32; 4] = core::mem::transmute(simd_or(
            u32x4([0b1010, 0b1100, 0xFFFF_0000, 0xF0F0]),
            u32x4([0b0110, 0b1010, 0x00FF_FF00, 0x0FF0]),
        ));
        if or_arr != [0b1110, 0b1110, 0xFFFF_FF00, 0xFFF0] {
            std::process::exit(6);
        }

        let xor_arr: [u32; 4] = core::mem::transmute(simd_xor(
            u32x4([0b1010, 0b1100, 0xFFFF_0000, 0xF0F0]),
            u32x4([0b0110, 0b1010, 0x00FF_FF00, 0x0FF0]),
        ));
        if xor_arr != [0b1100, 0b0110, 0xFF00_FF00, 0xFF00] {
            std::process::exit(7);
        }
    }
}
