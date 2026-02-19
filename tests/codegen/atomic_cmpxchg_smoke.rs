//! Smoke test for atomic cmpxchg lowering.

//@ run-pass
//@ exit-code: 0

// CHECK: #include <stdatomic.h>
// CHECK-LABEL: main
// CHECK: atomic_compare_exchange_strong_explicit

use std::sync::atomic::{AtomicU32, Ordering};

static VALUE: AtomicU32 = AtomicU32::new(5);

fn main() {
    VALUE.store(5, Ordering::SeqCst);

    let first = VALUE.compare_exchange(5, 8, Ordering::AcqRel, Ordering::Acquire);
    let second = VALUE.compare_exchange(5, 9, Ordering::AcqRel, Ordering::Acquire);

    if first != Ok(5) {
        std::process::exit(1);
    }
    if second != Err(8) {
        std::process::exit(2);
    }
}
