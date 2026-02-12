//! Test minimal repr(Rust) struct lowering.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 11

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

pub struct Pair {
    a: i32,
    b: i32,
}

// CHECK-LABEL: main
// CHECK: return
#[no_mangle]
pub fn main() -> i32 {
    let mut p = Pair { a: 10, b: 20 };
    let x = p.a;
    p.b = x + 1;
    p.b
}
