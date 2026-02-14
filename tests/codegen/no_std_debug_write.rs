//! Test no_std debug formatting through core::fmt::Write.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/no_std_debug_write.stdout

#![feature(rustc_private)]
#![no_std]
#![no_main]

extern crate libc;

use core::fmt::{self, Write};
use core::panic::PanicInfo;

#[derive(Debug)]
struct Pair {
    a: i32,
    b: i32,
}

struct Sink;

impl Write for Sink {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let len = if s.len() > i32::MAX as usize { i32::MAX } else { s.len() as i32 };
        unsafe {
            libc::printf(b"%.*s\0".as_ptr().cast(), len, s.as_ptr().cast::<i8>());
        }
        Ok(())
    }
}

#[panic_handler]
fn panic(_: &PanicInfo<'_>) -> ! {
    loop {}
}

// CHECK-LABEL: main
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let mut sink = Sink;
    let _ = write!(&mut sink, "{:?}\n", Pair { a: 1, b: 2 });
    0
}
