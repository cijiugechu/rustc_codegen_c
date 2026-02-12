//! Test local slice fat pointer materialization.

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
fn use_slice(s: &[u8]) -> i32 {
    s.len() as i32
}

#[inline(never)]
#[no_mangle]
pub fn local_slice_copy(s: &[u8]) -> i32 {
    let t = s;
    use_slice(t)
}

// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let data = [1_u8, 2, 3, 4];
    local_slice_copy(&data)
}
