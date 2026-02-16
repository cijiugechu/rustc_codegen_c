//! Test std::cell::Cell::replace path with formatting/println.

//@ run-pass
//@ check-stdout-file: tests/expect/std_cell_replace_print.stdout

use std::cell::Cell;

// CHECK-LABEL: main
fn main() {
    let x = Cell::new(10u32);
    let old = x.replace(99);
    println!("{} {}", old, x.get());
}
