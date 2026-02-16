//! Test GEP first index on struct pointee is treated as element stepping.

//@ run-pass
//@ exit-code: 20

#![no_std]
#![no_main]

use core::panic::PanicInfo;

struct W(u32);

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let arr = [W(10), W(20)];
    arr[1].0 as i32
}
