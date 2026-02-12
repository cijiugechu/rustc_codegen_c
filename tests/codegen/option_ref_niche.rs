//! Test Option reference niche encoding.

//@ run-pass
//@ exit-code: 9

#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

static X: u8 = 9;

#[inline(never)]
fn score(v: Option<&'static u8>) -> i32 {
    match v {
        Some(p) => *p as i32,
        None => 1,
    }
}

// CHECK-LABEL: score
// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    score(Some(&X))
}
