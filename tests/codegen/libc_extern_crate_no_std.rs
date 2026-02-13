//! Test calling libc::printf through extern crate libc in no_std.

//@ run-pass
//@ exit-code: 0

#![feature(rustc_private)]
#![no_std]
#![no_main]

extern crate libc;

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_: &PanicInfo<'_>) -> ! {
    loop {}
}

// CHECK-LABEL: main
// CHECK: printf
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let fmt = b"extern libc %d\n\0";
    unsafe {
        libc::printf(fmt.as_ptr().cast(), 42i32);
    }
    0
}
