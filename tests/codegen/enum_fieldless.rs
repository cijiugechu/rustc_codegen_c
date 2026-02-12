//! Test fieldless enum lowering through scalar discriminants.

//@ run-pass
//@ exit-code: 22

#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[repr(u8)]
enum E {
    A = 1,
    B = 2,
    C = 5,
}

#[inline(never)]
fn pick(x: u8) -> E {
    if x == 0 {
        E::A
    } else if x == 1 {
        E::B
    } else {
        E::C
    }
}

// CHECK-LABEL: pick
// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    match pick(1) {
        E::A => 11,
        E::B => 22,
        E::C => 55,
    }
}
