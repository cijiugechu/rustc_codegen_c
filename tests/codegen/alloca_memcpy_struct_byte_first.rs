//! Test struct copy_nonoverlapping keeps stack slots byte-backed.

//@ run-pass
//@ exit-code: 55

#![feature(core_intrinsics)]
#![allow(internal_features)]
#![no_std]
#![no_main]

use core::intrinsics;
use core::panic::PanicInfo;

#[repr(C)]
#[derive(Copy, Clone)]
struct Pair {
    a: u32,
    b: u32,
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[inline(never)]
fn do_copy() -> i32 {
    let src = [Pair { a: 11, b: 22 }, Pair { a: 33, b: 44 }];
    let mut dst = [Pair { a: 0, b: 0 }, Pair { a: 0, b: 0 }];
    unsafe {
        intrinsics::copy_nonoverlapping::<Pair>(src.as_ptr(), dst.as_mut_ptr(), 2);
    }
    (dst[0].a as i32) + (dst[1].b as i32)
}

// CHECK-LABEL: main
// CHECK: __builtin_memcpy
// CHECK: int8_t *_{{[0-9]+}} = (int8_t *)
#[no_mangle]
pub extern "C" fn main() -> i32 {
    do_copy()
}
