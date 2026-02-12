//! Test addition codegen for multiple primitive types.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 42

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

// CHECK-LABEL: add_i16
// CHECK-LABEL: add_i32
// CHECK-LABEL: add_i64
// CHECK-LABEL: add_u8
// CHECK-LABEL: main

// CHECK-LABEL: add_i16
// CHECK: int16_t _{{[0-9]+}} = (_0 + _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: add_i32
// CHECK: int32_t _{{[0-9]+}} = (_0 + _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: add_i64
// CHECK: int64_t _{{[0-9]+}} = (_0 + _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: add_u8
// CHECK: uint8_t _{{[0-9]+}} = (_0 + _1);
// CHECK: return _{{[0-9]+}};
#[no_mangle]
pub fn add_i32(x: i32, y: i32) -> i32 {
    x + y
}

#[no_mangle]
pub fn add_i64(x: i64, y: i64) -> i64 {
    x + y
}

#[no_mangle]
pub fn add_u8(x: u8, y: u8) -> u8 {
    x + y
}

#[no_mangle]
pub fn add_i16(x: i16, y: i16) -> i16 {
    x + y
}

// CHECK-LABEL: main
// CHECK: int32_t _{{[0-9]+}} = add_i32(2, 3);
// CHECK: int64_t _{{[0-9]+}} = add_i64(10, 20);
// CHECK: uint8_t _{{[0-9]+}} = add_u8(1, 2);
// CHECK: int16_t _{{[0-9]+}} = add_i16(1, 3);
// CHECK: return _{{[0-9]+}};
#[no_mangle]
pub fn main() -> i32 {
    let a = add_i32(2, 3);
    let b = add_i64(10, 20) as i32;
    let c = add_u8(1, 2) as i32;
    let d = add_i16(1, 3) as i32;
    a + b + c + d
}
