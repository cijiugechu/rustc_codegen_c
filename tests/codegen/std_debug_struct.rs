//! Test `fmt::DebugStruct` builder path.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_debug_struct.stdout

use core::fmt;

struct Demo {
    x: i32,
    y: i32,
}

impl fmt::Debug for Demo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Demo").field("x", &self.x).field("y", &self.y).finish()
    }
}

// CHECK-LABEL: main
fn main() {
    println!("{:?}", Demo { x: 1, y: 2 });
}
