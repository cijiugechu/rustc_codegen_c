//! Test by-value struct argument and return lowering at call boundaries.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 5

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[repr(C)]
pub struct Pair {
    a: i32,
    b: i32,
}

#[inline(never)]
#[no_mangle]
pub fn add1(p: Pair) -> Pair {
    Pair { a: p.a + 1, b: p.b + 1 }
}

// CHECK: struct __rcgenc_abi_tuple_
// CHECK: add1(int32_t _0, int32_t _1);
// CHECK-LABEL: int32_t main()
// CHECK: add1(1, 2)
// CHECK: .f0
// CHECK: .f1
#[no_mangle]
pub fn main() -> i32 {
    let p = Pair { a: 1, b: 2 };
    let q = add1(p);
    q.a + q.b
}
