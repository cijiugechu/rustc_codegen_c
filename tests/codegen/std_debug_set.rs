//! Test `fmt::DebugSet` builder path.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_debug_set.stdout

use core::fmt;

struct Demo;

impl fmt::Debug for Demo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut set = f.debug_set();
        set.entry(&1);
        set.entry(&2);
        set.finish()
    }
}

// CHECK-LABEL: main
fn main() {
    println!("{:?}", Demo);
}
