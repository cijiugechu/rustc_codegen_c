//! Test subtraction codegen for multiple primitive integer types.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 49

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

// CHECK-LABEL: int32_t main(
// CHECK-LABEL: int16_t sub_i16(
// CHECK-LABEL: int32_t sub_i32(
// CHECK-LABEL: int64_t sub_i64(
// CHECK-LABEL: uint8_t sub_u8(

// CHECK-LABEL: int32_t main(
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: int16_t sub_i16(
// CHECK: int16_t _{{[0-9]+}} = (_0 - _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: int32_t sub_i32(
// CHECK: int32_t _{{[0-9]+}} = (_0 - _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: int64_t sub_i64(
// CHECK: int64_t _{{[0-9]+}} = (_0 - _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: uint8_t sub_u8(
// CHECK: uint8_t _{{[0-9]+}} = (_0 - _1);
// CHECK: return _{{[0-9]+}};
#[no_mangle]
pub fn sub_i32(x: i32, y: i32) -> i32 {
    x - y
}

#[no_mangle]
pub fn sub_i64(x: i64, y: i64) -> i64 {
    x - y
}

#[no_mangle]
pub fn sub_u8(x: u8, y: u8) -> u8 {
    x - y
}

#[no_mangle]
pub fn sub_i16(x: i16, y: i16) -> i16 {
    x - y
}

#[no_mangle]
pub fn main() -> i32 {
    let a = sub_i32(7, 2);
    let b = sub_i64(40, 10) as i32;
    let c = sub_u8(9, 4) as i32;
    let d = sub_i16(12, 3) as i32;
    a + b + c + d
}
