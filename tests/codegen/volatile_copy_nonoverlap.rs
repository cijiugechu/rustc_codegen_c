//! Test volatile_copy_nonoverlapping_memory lowers to memcpy path.

//@ run-pass
//@ exit-code: 13

#![feature(core_intrinsics)]
#![no_std]
#![no_main]

use core::intrinsics;
use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[inline(never)]
fn do_volatile_copy() -> i32 {
    let src = [5u8, 6, 7, 8];
    let mut dst = [0u8; 4];
    unsafe {
        intrinsics::volatile_copy_nonoverlapping_memory::<u8>(
            dst.as_mut_ptr(),
            src.as_ptr(),
            4,
        );
        let a = dst[0] as i32;
        let b = dst[3] as i32;
        a + b
    }
}

// CHECK: __builtin_memcpy
#[no_mangle]
pub extern "C" fn main() -> i32 {
    do_volatile_copy()
}
