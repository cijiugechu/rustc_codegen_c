//! Smoke test for atomic RMW lowering.

//@ run-pass
//@ exit-code: 0

// CHECK: #include <stdatomic.h>
// CHECK-LABEL: main
// CHECK: atomic_fetch_sub_explicit
// CHECK: atomic_fetch_xor_explicit

use std::sync::atomic::{AtomicU32, Ordering};

static VALUE: AtomicU32 = AtomicU32::new(3);

fn main() {
    VALUE.store(3, Ordering::SeqCst);

    let old_add = VALUE.fetch_add(2, Ordering::SeqCst);
    let old_sub = VALUE.fetch_sub(1, Ordering::SeqCst);
    let old_or = VALUE.fetch_or(0b1000, Ordering::SeqCst);
    let old_and = VALUE.fetch_and(0b1110, Ordering::SeqCst);
    let old_xor = VALUE.fetch_xor(0b0010, Ordering::SeqCst);
    let old_max = VALUE.fetch_max(20, Ordering::SeqCst);
    let old_min = VALUE.fetch_min(6, Ordering::SeqCst);

    if old_add != 3
        || old_sub != 5
        || old_or != 4
        || old_and != 12
        || old_xor != 12
        || old_max != 14
        || old_min != 20
    {
        std::process::exit(1);
    }

    if VALUE.load(Ordering::SeqCst) != 6 {
        std::process::exit(2);
    }
}
