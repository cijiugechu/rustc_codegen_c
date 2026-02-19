//! Smoke test for Arc lowering path.

//@ run-pass
//@ exit-code: 0

use std::sync::Arc;

// CHECK-LABEL: main
// CHECK-NOT: only struct ADTs are supported
fn main() {
    let value = Arc::new(41usize);
    let cloned = Arc::clone(&value);
    if *cloned != 41 {
        std::process::exit(1);
    }
    if Arc::strong_count(&value) != 2 {
        std::process::exit(1);
    }
}
