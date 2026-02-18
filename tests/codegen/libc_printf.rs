//! Test calling libc printf from mini_core.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 0

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

// CHECK: #include <stdio.h>
// CHECK-LABEL: main
// CHECK: printf
#[no_mangle]
pub fn main() -> i32 {
    let fmt: [u8; 16] = [
        b'H', b'e', b'l', b'l', b'o', b' ', b'W', b'o', b'r', b'l', b'd', b' ', b'%', b'd', b'\n',
        0,
    ];
    unsafe {
        mini_core::libc::printf(&fmt as *const [u8; 16] as *const u8 as *const i8, 42i32);
    }
    0
}
