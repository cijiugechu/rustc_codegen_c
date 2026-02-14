//! Test `fmt::DebugTuple` builder path.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_debug_tuple.stdout

use core::fmt;

struct Demo(i32, i32);

impl fmt::Debug for Demo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Demo").field(&self.0).field(&self.1).finish()
    }
}

// CHECK-LABEL: main
fn main() {
    println!("{:?}", Demo(1, 2));
}
