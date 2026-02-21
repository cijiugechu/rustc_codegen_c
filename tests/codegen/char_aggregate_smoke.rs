//! Ensure Rust `char` is lowered as a 32-bit scalar in aggregate fields.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 0

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[repr(C)]
pub struct ReprCChar {
    char_c: char,
    val_c: u32,
}

pub struct ReprRustChar {
    char_r: char,
    val_r: u32,
}

// CHECK: struct ReprCChar {
// CHECK: uint32_t char_c;
#[no_mangle]
pub fn main() -> i32 {
    let mut c = ReprCChar { char_c: 'A', val_c: 1 };
    let mut r = ReprRustChar { char_r: 'B', val_r: 2 };
    let mut t = ('C', 3u32);
    let mut h = ('文', 4u32);

    c.char_c = 'D';
    r.char_r = 'E';
    t.0 = 'F';
    h.0 = '汉';

    let sum = (c.char_c as u32)
        + (r.char_r as u32)
        + (t.0 as u32)
        + (h.0 as u32)
        + c.val_c
        + r.val_r
        + t.1
        + h.1;
    (sum as i32) - ((68 + 69 + 70 + 27721) + 1 + 2 + 3 + 4)
}
