//! Test dynamic first index on struct arrays does not get interpreted as struct field index.

//@ run-pass
//@ exit-code: 4

#![no_std]
#![no_main]

use core::hint::black_box;
use core::panic::PanicInfo;

struct P {
    a: i32,
    b: i32,
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let arr = [P { a: 1, b: 2 }, P { a: 3, b: 4 }];
    let i = black_box(1usize);
    arr[i].b
}
