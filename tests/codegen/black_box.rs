//! Test black_box intrinsic override lowers to helper observe call.

//@ run-pass
//@ exit-code: 77

#![no_std]
#![no_main]

use core::hint::black_box;
use core::panic::PanicInfo;

#[inline(never)]
fn use_black_box(v: i32) -> i32 {
    black_box(v)
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

// CHECK-LABEL: main
// CHECK: __rust_black_box_observe(
// CHECK: return _{{[0-9]+}};
#[no_mangle]
pub extern "C" fn main() -> i32 {
    use_black_box(77)
}
