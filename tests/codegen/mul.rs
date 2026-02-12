//! Test multiplication codegen for multiple primitive integer types.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 71

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

// CHECK-LABEL: int32_t main(
// CHECK-LABEL: int16_t mul_i16(
// CHECK-LABEL: int32_t mul_i32(
// CHECK-LABEL: int64_t mul_i64(
// CHECK-LABEL: uint8_t mul_u8(

// CHECK-LABEL: int32_t main(
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: int16_t mul_i16(
// CHECK: int16_t _{{[0-9]+}} = (_0 * _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: int32_t mul_i32(
// CHECK: int32_t _{{[0-9]+}} = (_0 * _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: int64_t mul_i64(
// CHECK: int64_t _{{[0-9]+}} = (_0 * _1);
// CHECK: return _{{[0-9]+}};
// CHECK-LABEL: uint8_t mul_u8(
// CHECK: uint8_t _{{[0-9]+}} = (_0 * _1);
// CHECK: return _{{[0-9]+}};
#[no_mangle]
pub fn mul_i32(x: i32, y: i32) -> i32 {
    x * y
}

#[no_mangle]
pub fn mul_i64(x: i64, y: i64) -> i64 {
    x * y
}

#[no_mangle]
pub fn mul_u8(x: u8, y: u8) -> u8 {
    x * y
}

#[no_mangle]
pub fn mul_i16(x: i16, y: i16) -> i16 {
    x * y
}

#[no_mangle]
pub fn main() -> i32 {
    let a = mul_i32(3, 4);
    let b = mul_i64(5, 6) as i32;
    let c = mul_u8(2, 7) as i32;
    let d = mul_i16(3, 5) as i32;
    a + b + c + d
}
