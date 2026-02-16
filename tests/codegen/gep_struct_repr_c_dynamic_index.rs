//! Test repr(C) struct array dynamic index projection.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 42

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[repr(C)]
pub struct Pair {
    a: i32,
    b: i32,
}

// CHECK-LABEL: main
#[no_mangle]
pub fn main() -> i32 {
    let arr = [Pair { a: 1, b: 2 }, Pair { a: 20, b: 22 }];
    let mut i: usize = 0;
    i = i + 1;
    arr[i].a + arr[i].b
}
