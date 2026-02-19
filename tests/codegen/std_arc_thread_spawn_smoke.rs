//! Smoke test for Arc + thread spawn static materialization path.

//@ run-pass
//@ exit-code: 0

use std::sync::Arc;
use std::thread;

// CHECK-LABEL: main
// CHECK-NOT: only struct ADTs are supported
// CHECK-NOT: use of undeclared identifier
// CHECK: extern uint8_t {{.*MIN.*}}[] __asm__(
// CHECK-NOT: {{.*MIN.*}}[1]
fn main() {
    let value = Arc::new(41usize);
    let worker_input = Arc::clone(&value);
    let handle = thread::spawn(move || *worker_input + 1);
    let result = handle.join().unwrap();
    if result != 42 {
        std::process::exit(1);
    }
}
