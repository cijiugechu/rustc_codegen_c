//! Test std::mem::replace semantics without formatting paths.

//@ run-pass
//@ exit-code: 0

// CHECK-LABEL: main
fn main() {
    let mut x = 10u32;
    let old = std::mem::replace(&mut x, 99);
    if old != 10 || x != 99 {
        std::process::abort();
    }
}
