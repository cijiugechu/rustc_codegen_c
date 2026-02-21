//! Test scalar saturating_add/saturating_sub intrinsic lowering and runtime semantics.

//@ run-pass
//@ exit-code: 0

#![feature(core_intrinsics)]
#![allow(internal_features)]

use core::intrinsics;

// CHECK: __builtin_add_overflow
// CHECK: __builtin_sub_overflow

#[inline(never)]
fn sat_add_u8(lhs: u8, rhs: u8) -> u8 {
    intrinsics::saturating_add(lhs, rhs)
}

#[inline(never)]
fn sat_sub_u8(lhs: u8, rhs: u8) -> u8 {
    intrinsics::saturating_sub(lhs, rhs)
}

#[inline(never)]
fn sat_add_u64(lhs: u64, rhs: u64) -> u64 {
    intrinsics::saturating_add(lhs, rhs)
}

#[inline(never)]
fn sat_sub_u64(lhs: u64, rhs: u64) -> u64 {
    intrinsics::saturating_sub(lhs, rhs)
}

#[inline(never)]
fn sat_add_i8(lhs: i8, rhs: i8) -> i8 {
    intrinsics::saturating_add(lhs, rhs)
}

#[inline(never)]
fn sat_sub_i8(lhs: i8, rhs: i8) -> i8 {
    intrinsics::saturating_sub(lhs, rhs)
}

#[inline(never)]
fn sat_add_i64(lhs: i64, rhs: i64) -> i64 {
    intrinsics::saturating_add(lhs, rhs)
}

#[inline(never)]
fn sat_sub_i64(lhs: i64, rhs: i64) -> i64 {
    intrinsics::saturating_sub(lhs, rhs)
}

fn main() {
    if sat_add_u8(250, 10) != u8::MAX {
        std::process::exit(1);
    }
    if sat_sub_u8(3, 10) != 0 {
        std::process::exit(2);
    }
    if sat_add_u64(u64::MAX - 2, 10) != u64::MAX {
        std::process::exit(3);
    }
    if sat_sub_u64(5, 9) != 0 {
        std::process::exit(4);
    }

    if sat_add_i8(120, 20) != i8::MAX {
        std::process::exit(5);
    }
    if sat_add_i8(-120, -20) != i8::MIN {
        std::process::exit(6);
    }
    if sat_sub_i8(120, -20) != i8::MAX {
        std::process::exit(7);
    }
    if sat_sub_i8(-120, 20) != i8::MIN {
        std::process::exit(8);
    }

    if sat_add_i64(i64::MAX - 1, 10) != i64::MAX {
        std::process::exit(9);
    }
    if sat_add_i64(i64::MIN + 1, -10) != i64::MIN {
        std::process::exit(10);
    }
    if sat_sub_i64(i64::MIN + 1, 10) != i64::MIN {
        std::process::exit(11);
    }
    if sat_sub_i64(i64::MAX - 1, -10) != i64::MAX {
        std::process::exit(12);
    }
}
