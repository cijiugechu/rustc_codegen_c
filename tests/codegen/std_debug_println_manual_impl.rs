//! Test `println!("{:?}", ..)` with a manual `Debug` impl.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_debug_println_manual_impl.stdout

use core::fmt;

struct Counter {
    value: i32,
}

impl fmt::Debug for Counter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _ = self.value;
        f.write_str("Counter")
    }
}

// CHECK-LABEL: main
fn main() {
    println!("{:?}", Counter { value: 7 });
}
