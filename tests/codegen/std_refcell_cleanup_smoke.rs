//! Test std::cell::RefCell path that previously hit cleanup-callsite todo.

//@ run-pass
//@ exit-code: 0

use std::cell::RefCell;

#[inline(never)]
fn touch(x: usize) {
    std::hint::black_box(x);
}

struct CleanupProbe;

impl Drop for CleanupProbe {
    fn drop(&mut self) {
        touch(1);
    }
}

// CHECK-LABEL: main
fn main() {
    let _probe = CleanupProbe;
    let cell = RefCell::new(String::from("abc"));
    {
        let mut s = cell.borrow_mut();
        s.push_str("def");
    }

    let expected = String::from("abcdef");
    if *cell.borrow() != expected {
        std::process::abort();
    }
}
