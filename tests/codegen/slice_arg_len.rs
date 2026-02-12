//! Test passing a slice fat pointer as function argument.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 0

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[inline(never)]
fn takes_slice(_s: &[u8]) -> i32 {
    0
}

#[inline(never)]
#[no_mangle]
pub fn forward_slice(s: &[u8]) -> i32 {
    takes_slice(s)
}

// CHECK-LABEL: takes_slice
// CHECK-LABEL: main
#[no_mangle]
pub fn main() -> i32 {
    0
}
