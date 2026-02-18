//! Test raw_eq intrinsic lowering strategy and runtime semantics.

//@ run-pass
//@ exit-code: 0

#![feature(core_intrinsics)]
#![allow(internal_features)]

use core::intrinsics;

#[repr(C)]
pub struct Pair {
    a: u64,
    b: u64,
}

// CHECK-LABEL: raw_eq_u32
// CHECK-NOT: __builtin_memcmp
#[inline(never)]
#[no_mangle]
pub fn raw_eq_u32(lhs: &u32, rhs: &u32) -> bool {
    unsafe { intrinsics::raw_eq(lhs, rhs) }
}

// CHECK-LABEL: raw_eq_pair
// CHECK-NOT: __builtin_memcmp
#[inline(never)]
#[no_mangle]
pub fn raw_eq_pair(lhs: &Pair, rhs: &Pair) -> bool {
    unsafe { intrinsics::raw_eq(lhs, rhs) }
}

// CHECK-LABEL: raw_eq_large
// CHECK: __builtin_memcmp
#[inline(never)]
#[no_mangle]
pub fn raw_eq_large(lhs: &[u8; 33], rhs: &[u8; 33]) -> bool {
    unsafe { intrinsics::raw_eq(lhs, rhs) }
}

#[inline(never)]
#[no_mangle]
pub fn raw_eq_zst(lhs: &(), rhs: &()) -> bool {
    unsafe { intrinsics::raw_eq(lhs, rhs) }
}

fn main() {
    let a = 42u32;
    let b = 42u32;
    let c = 7u32;
    if !raw_eq_u32(&a, &b) || raw_eq_u32(&a, &c) {
        std::process::exit(1);
    }

    let p1 = Pair { a: 1, b: 2 };
    let p2 = Pair { a: 1, b: 2 };
    let p3 = Pair { a: 1, b: 3 };
    if !raw_eq_pair(&p1, &p2) || raw_eq_pair(&p1, &p3) {
        std::process::exit(2);
    }

    let mut l1 = [0u8; 33];
    let mut l2 = [0u8; 33];
    l1[32] = 9;
    l2[32] = 9;
    if !raw_eq_large(&l1, &l2) {
        std::process::exit(3);
    }
    l2[0] = 1;
    if raw_eq_large(&l1, &l2) {
        std::process::exit(4);
    }

    if !raw_eq_zst(&(), &()) {
        std::process::exit(5);
    }
}
