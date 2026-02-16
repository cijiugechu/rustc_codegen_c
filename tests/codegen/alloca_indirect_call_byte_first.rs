//! Test indirect by-value struct call uses byte-backed stack slots.

//@ run-pass
//@ exit-code: 0

#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[repr(C)]
#[derive(Copy, Clone)]
struct Big {
    a: u64,
    b: u64,
    c: u64,
    d: u64,
    e: u64,
}

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[inline(never)]
fn bump(mut v: Big) -> Big {
    v.a = v.a + 1;
    v.e = v.e + 2;
    v
}

// CHECK-LABEL: bump
// CHECK-LABEL: main
// CHECK: uint8_t _{{[0-9]+}}[
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let input = Big { a: 1, b: 2, c: 3, d: 4, e: 5 };
    let out = bump(input);
    if out.a == 2 && out.e == 7 { 0 } else { 1 }
}
