//! Test `fmt::DebugList` builder path.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_debug_list.stdout

use core::fmt;

struct Demo;

impl fmt::Debug for Demo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = f.debug_list();
        list.entry(&1);
        list.entry(&2);
        list.finish()
    }
}

// CHECK-LABEL: main
fn main() {
    println!("{:?}", Demo);
}
