//! Test std::mem::replace path with formatting/println.

//@ run-pass
//@ check-stdout-file: tests/expect/std_mem_replace_print.stdout

// CHECK-LABEL: main
fn main() {
    let mut x = 10u32;
    let old = std::mem::replace(&mut x, 99);
    println!("{} {}", old, x);
}
