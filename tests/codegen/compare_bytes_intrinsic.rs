//! Test compare_bytes intrinsic lowering and runtime sign semantics.

//@ run-pass
//@ exit-code: 55

#![feature(core_intrinsics)]
#![allow(internal_features)]
#![no_std]
#![no_main]

use core::intrinsics;
use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[inline(never)]
fn cmp_sign(a: &[u8], b: &[u8]) -> i32 {
    let cmp = unsafe { intrinsics::compare_bytes(a.as_ptr(), b.as_ptr(), a.len()) };
    if cmp < 0 {
        -1
    } else if cmp > 0 {
        1
    } else {
        0
    }
}

// CHECK: __builtin_memcmp
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let eq = cmp_sign(&[1, 2, 3], &[1, 2, 3]);
    let lt = cmp_sign(&[1, 2, 3], &[1, 2, 4]);
    let gt = cmp_sign(&[1, 2, 4], &[1, 2, 3]);

    if eq == 0 && lt < 0 && gt > 0 { 55 } else { 1 }
}
