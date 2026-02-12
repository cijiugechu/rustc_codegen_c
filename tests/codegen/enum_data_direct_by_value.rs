//! Test direct-tag data enum passed and returned by value.

//@ run-pass
//@ exit-code: 14

#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[repr(C, u8)]
enum E {
    A(u32, u32),
    B(u16),
}

#[inline(never)]
fn bump(e: E) -> E {
    match e {
        E::A(x, y) => E::A(x + 1, y + 1),
        E::B(v) => E::B(v + 2),
    }
}

#[inline(never)]
fn score(e: E) -> i32 {
    match e {
        E::A(x, y) => (x + y) as i32,
        E::B(v) => v as i32,
    }
}

// CHECK-LABEL: bump
// CHECK-LABEL: score
#[no_mangle]
pub extern "C" fn main() -> i32 {
    score(bump(E::B(12)))
}
