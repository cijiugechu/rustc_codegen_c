//! Regression test for zero-sized union layouts (e.g. MaybeUninit<ZST>).

//@ run-pass
//@ exit-code: 0

use core::mem::MaybeUninit;

struct Marker;

#[inline(never)]
fn build() -> MaybeUninit<Marker> {
    MaybeUninit::uninit()
}

// CHECK-LABEL: main
fn main() {
    let value = build();
    let _ = value;
    assert_eq!(core::mem::size_of::<MaybeUninit<Marker>>(), 0);
}
