//! Test by-value tuple argument/return lowering for non-scalar-pair tuples.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 9

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[inline(never)]
#[no_mangle]
pub fn add1(p: (i32, i32, i32)) -> (i32, i32, i32) {
    (p.0 + 1, p.1 + 1, p.2 + 1)
}

// CHECK: struct __rcgenc_tuple_
// CHECK: void add1(struct __rcgenc_tuple_
// CHECK-LABEL: int32_t main()
// CHECK: add1(
#[no_mangle]
pub fn main() -> i32 {
    let q = add1((1, 2, 3));
    q.0 + q.1 + q.2
}
