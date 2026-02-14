//! Test `println!("{:?}", ..)` debug formatting path.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_debug_println.stdout

#[derive(Debug)]
struct Pair {
    a: i32,
    b: i32,
}

// CHECK-LABEL: main
fn main() {
    println!("{:?}", Pair { a: 1, b: 2 });
}
