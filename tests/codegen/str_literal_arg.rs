//! Test passing a string literal fat pointer as function argument.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 0

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[inline(never)]
fn takes_str(_s: &str) -> i32 {
    0
}

// CHECK-LABEL: takes_str
// CHECK-LABEL: main
// CHECK: ((uint8_t *)"hello"),
// CHECK: 5
#[no_mangle]
pub fn main() -> i32 {
    takes_str("hello")
}
