//! Test copy with overlap lowers to memmove.

//@ run-pass
//@ exit-code: 11

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
fn do_copy_overlap() -> i32 {
    let mut buf = [1u8, 2, 3, 4, 5];
    unsafe {
        intrinsics::copy::<u8>(buf.as_ptr(), buf.as_mut_ptr().add(1), 4);
        (buf[0] as i32) + (buf[1] as i32) + (buf[2] as i32) + (buf[3] as i32) + (buf[4] as i32)
    }
}

// CHECK: __builtin_memmove
#[no_mangle]
pub extern "C" fn main() -> i32 {
    do_copy_overlap()
}
