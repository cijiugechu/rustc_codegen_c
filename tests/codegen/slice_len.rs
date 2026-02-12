//! Test local slice fat pointer materialization.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 0

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[inline(never)]
fn use_slice(_s: &[u8]) -> i32 {
    0
}

#[inline(never)]
#[no_mangle]
pub fn local_slice_copy(s: &[u8]) -> i32 {
    let t = s;
    use_slice(t)
}

// CHECK-LABEL: main
#[no_mangle]
pub fn main() -> i32 {
    0
}
