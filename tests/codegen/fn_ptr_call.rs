//! Test function pointer parameter lowering and call emission.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 41

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

#[inline(never)]
#[no_mangle]
fn add_one(x: i32) -> i32 {
    x + 1
}

#[inline(never)]
#[no_mangle]
fn apply(f: fn(i32) -> i32, x: i32) -> i32 {
    f(x)
}

// CHECK-LABEL: apply
// CHECK: int32_t (*{{[[:alnum:]_]*}})(int32_t)
// CHECK-NOT: int32_t apply(uint8_t *
// CHECK: = ((int32_t (*)(int32_t)) _0)(_1);
#[no_mangle]
pub fn main() -> i32 {
    apply(add_one, 40)
}
