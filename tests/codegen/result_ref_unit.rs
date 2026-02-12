//! Test Result with pointer/unit niche-style layout.

//@ run-pass
//@ exit-code: 7

#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

static X: u8 = 7;

#[inline(never)]
fn score(v: Result<&'static u8, ()>) -> i32 {
    match v {
        Ok(p) => *p as i32,
        Err(()) => 33,
    }
}

// CHECK-LABEL: score
// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    score(Ok(&X))
}
