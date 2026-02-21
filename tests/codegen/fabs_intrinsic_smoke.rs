//! Regression test for unary float intrinsics that must be codegen-overridden.

//@ run-pass
//@ exit-code: 0

#![feature(core_intrinsics)]
#![allow(internal_features)]

use core::intrinsics;

// CHECK: __builtin_fabsf
// CHECK: __builtin_fabs

#[inline(never)]
fn abs32(x: f32) -> f32 {
    intrinsics::fabsf32(x)
}

#[inline(never)]
fn abs64(x: f64) -> f64 {
    intrinsics::fabsf64(x)
}

fn main() {
    if abs32(-3.5) != 3.5 {
        std::process::exit(1);
    }
    if abs64(-7.25) != 7.25 {
        std::process::exit(2);
    }
}
