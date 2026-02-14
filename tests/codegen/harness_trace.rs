//! Harness-based runtime trace test.

//@ aux-build:test_harness.rs
//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/harness_trace.stdout

#![no_std]
#![no_main]

extern crate test_harness;

use core::panic::PanicInfo;

#[derive(Debug)]
struct Pair {
    lhs: i32,
    rhs: i32,
}

#[panic_handler]
fn panic(_: &PanicInfo<'_>) -> ! {
    loop {}
}

// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let a = 2 + 3;
    let b = a * 2;
    let marker = 'Z';
    let series = [1i32, 2, 3];
    let pair = Pair { lhs: a, rhs: b };

    test_harness::begin("harness_trace");
    test_harness::trace!("a", a);
    test_harness::trace!("b", b);
    test_harness::trace!("a less than b", a < b);
    test_harness::trace!("marker", marker);
    test_harness::trace!("pair", pair);
    test_harness::assert_eq!("a should be 5", 5, a);
    test_harness::assert_eq!("b should be 10", 10, b);
    test_harness::assert_eq!("a less than b", true, a < b);
    test_harness::assert_eq!("marker should be Z", 'Z', marker);
    test_harness::assert_eq!("series[2] should be 3", 3, series[2]);
    test_harness::finish()
}
