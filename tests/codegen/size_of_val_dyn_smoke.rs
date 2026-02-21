//! Regression for size_of_val lowering paths that use integer-typed select.

//@ run-pass
//@ exit-code: 0

// CHECK: size_of_val_dyn_smoke

use core::fmt::Debug;

#[inline(never)]
fn dyn_size(v: &dyn Debug) -> usize {
    core::mem::size_of_val(v)
}

fn main() {
    let value: &dyn Debug = &123u32;
    if dyn_size(value) != 4 {
        std::process::exit(1);
    }
}
