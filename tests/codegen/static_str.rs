//! Test reading a `static &str` value.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 0

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

static S: &str = "hello";

#[inline(never)]
fn takes_str(_s: &str) -> i32 {
    0
}

// CHECK-LABEL: main
// CHECK: "hello"
// CHECK: , 5
// CHECK-LABEL: main
// CHECK: *(uint8_t **) ((uint8_t *)&
// CHECK: = &_1[8];
// CHECK: *(uint64_t *) _2;
#[no_mangle]
pub fn main() -> i32 {
    takes_str(S)
}
