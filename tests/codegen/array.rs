//! Test primitive integer array allocation codegen without indexing.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 0

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

// CHECK-LABEL: main
// CHECK: uint8_t _{{[0-9]+}}[12];
// CHECK: return 0;
#[no_mangle]
pub fn main() -> i32 {
    let _arr_i32: [i32; 3] = [1, 2, 3];
    let _arr_u8: [u8; 4] = [10, 20, 30, 40];
    0
}
