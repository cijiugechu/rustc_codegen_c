//! Test forwarding a string literal fat pointer through one call.

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

#[inline(never)]
fn forward_str(s: &str) -> i32 {
    let t = s;
    takes_str(t)
}

// CHECK-LABEL: forward_str
// CHECK-LABEL: main
// CHECK: ((uint8_t *)"world"),
// CHECK: 5
#[no_mangle]
pub fn main() -> i32 {
    forward_str("world")
}
