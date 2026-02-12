//! Test repr(Rust) struct with alignment-sensitive fields.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 21

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

pub struct Mixed {
    a: u8,
    b: i32,
}

// CHECK-LABEL: main
// CHECK: return
#[no_mangle]
pub fn main() -> i32 {
    let mut s = Mixed { a: 1, b: 20 };
    let x = s.a;
    s.a = x + 1;
    s.b = s.b + 1;
    s.b
}
