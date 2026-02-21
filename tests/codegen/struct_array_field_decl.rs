//! Regression test for repr(C) struct fields with array types.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 5

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[repr(C)]
pub struct Packet {
    bytes: [u8; 4],
    tag: i32,
}

// CHECK-NOT: uint8_t [4]
// CHECK-LABEL: main
// CHECK: uint8_t _{{[0-9]+}}[4];
#[no_mangle]
pub fn main() -> i32 {
    let packet = Packet { bytes: [1, 2, 3, 4], tag: 5 };
    packet.tag
}
