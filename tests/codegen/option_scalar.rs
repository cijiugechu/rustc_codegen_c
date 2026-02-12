//! Test Option scalar enum lowering.

//@ run-pass
//@ exit-code: 4

#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[inline(never)]
fn score(v: Option<u8>) -> i32 {
    match v {
        Some(x) => x as i32,
        None => 17,
    }
}

// CHECK-LABEL: score
// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    score(Some(4))
}
