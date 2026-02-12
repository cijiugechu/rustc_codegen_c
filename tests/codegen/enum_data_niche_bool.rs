//! Test bool niche enum discriminant decoding with by-value call boundaries.

//@ run-pass
//@ exit-code: 33

#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

enum N {
    V1 { f: bool },
    V2,
    V3,
}

#[inline(never)]
fn choose(x: bool) -> N {
    if x {
        N::V1 { f: true }
    } else {
        N::V3
    }
}

#[inline(never)]
fn pass(e: N) -> N {
    match e {
        N::V1 { f } => N::V1 { f },
        N::V2 => N::V2,
        N::V3 => N::V3,
    }
}

#[inline(never)]
fn score(e: N) -> i32 {
    match pass(e) {
        N::V1 { f: true } => 11,
        N::V1 { f: false } => 12,
        N::V2 => 22,
        N::V3 => 33,
    }
}

// CHECK-LABEL: pass
// CHECK-LABEL: score
#[no_mangle]
pub extern "C" fn main() -> i32 {
    score(choose(false))
}
