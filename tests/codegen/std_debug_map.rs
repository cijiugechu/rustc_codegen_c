//! Test `fmt::DebugMap` builder path.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_debug_map.stdout

use core::fmt;

struct Demo;

impl fmt::Debug for Demo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut map = f.debug_map();
        map.entry(&"a", &1);
        map.entry(&"b", &2);
        map.finish()
    }
}

// CHECK-LABEL: main
fn main() {
    println!("{:?}", Demo);
}
