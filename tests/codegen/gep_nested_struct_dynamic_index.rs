//! Test dynamic first index followed by nested field projections.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 7

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

struct Inner {
    x: i32,
    y: i32,
}

struct Outer {
    tag: i32,
    inner: Inner,
}

// CHECK-LABEL: main
// CHECK: int8_t _{{[0-9]+}}[
#[no_mangle]
pub fn main() -> i32 {
    let arr = [
        Outer { tag: 1, inner: Inner { x: 2, y: 3 } },
        Outer { tag: 4, inner: Inner { x: 5, y: 7 } },
    ];
    let mut i: usize = 0;
    i = i + 1;
    arr[i].inner.y
}
