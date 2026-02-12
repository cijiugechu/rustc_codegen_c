//! Test integer addition codegen for primitive integers.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 5

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

// CHECK-LABEL: add
// CHECK-LABEL: main
// CHECK-LABEL: add
// CHECK: int32_t _{{[0-9]+}} = (_0 + _1);
// CHECK: return _{{[0-9]+}};
#[no_mangle]
pub fn add(x: i32, y: i32) -> i32 {
    x + y
}

// CHECK-LABEL: main
// CHECK: int32_t _{{[0-9]+}} = add(2, 3);
// CHECK: return _{{[0-9]+}};
#[no_mangle]
pub fn main() -> i32 {
    add(2, 3)
}
