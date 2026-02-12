//! Test array constant indexing codegen.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 21

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

// CHECK-LABEL: main
// CHECK: [1]
// CHECK: [2]
// CHECK: if (
// CHECK: __rust_panic_bounds_check
// CHECK: return
#[no_mangle]
pub fn main() -> i32 {
    let mut arr: [i32; 3] = [10, 20, 30];
    let x = arr[1];
    arr[2] = x + 1;
    arr[2]
}
