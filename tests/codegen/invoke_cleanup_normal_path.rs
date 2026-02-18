//@ run-pass
//@ exit-code: 0

use std::cell::Cell;

// CHECK-LABEL: invoke_with_cleanup_edge

struct DropFlag<'a>(&'a Cell<i32>);

impl Drop for DropFlag<'_> {
    fn drop(&mut self) {
        self.0.set(self.0.get() + 1);
    }
}

#[inline(never)]
fn callee(x: i32) -> i32 {
    x + 1
}

#[inline(never)]
fn invoke_with_cleanup_edge(drops: &Cell<i32>) -> i32 {
    let _guard = DropFlag(drops);
    // This call carries a MIR cleanup edge because `_guard` must be dropped on unwind.
    callee(40)
}

fn main() {
    let drops = Cell::new(0);
    let ret = invoke_with_cleanup_edge(&drops);
    if ret == 41 && drops.get() == 1 {
        std::process::exit(0);
    }
    std::process::exit(1);
}
