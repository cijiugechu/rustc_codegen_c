//! Test minimal repr(C) struct lowering.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 21

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[repr(C)]
pub struct Pair {
    a: i32,
    b: i32,
}

// CHECK: struct Pair
// CHECK: int32_t a;
// CHECK: int32_t b;
// CHECK-LABEL: main
// CHECK: return
#[no_mangle]
pub fn main() -> i32 {
    let a = 10;
    let b = 20;
    let mut p = Pair { a, b };
    let x = p.a;
    p.b = x + 1;
    p.a + p.b
}
