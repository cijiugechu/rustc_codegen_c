//! Test std::cell::Cell::replace semantics without formatting paths.

//@ run-pass
//@ exit-code: 0

use std::cell::Cell;

// CHECK-LABEL: main
fn main() {
    let x = Cell::new(10u32);
    let old = x.replace(99);
    if old != 10 || x.get() != 99 {
        std::process::abort();
    }
}
