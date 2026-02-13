//! Test copy_nonoverlapping lowers to memcpy.

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
fn do_copy() -> i32 {
    let src = [11u8, 22, 33, 44];
    let mut dst = [0u8; 4];
    unsafe {
        intrinsics::copy_nonoverlapping::<u8>(src.as_ptr(), dst.as_mut_ptr(), 4);
        let a = dst[0] as i32;
        let b = dst[3] as i32;
        a + b
    }
}

// CHECK: __builtin_memcpy
#[no_mangle]
pub extern "C" fn main() -> i32 {
    do_copy()
}
