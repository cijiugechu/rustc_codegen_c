//! Regression test for call-argument coercion on pointer parameters.

//@ run-pass
//@ exit-code: 0

#[inline(never)]
#[no_mangle]
pub fn default_box_str_len() -> usize {
    let boxed: Box<str> = Default::default();
    assert!(boxed.is_empty());
    boxed.len()
}

// CHECK-LABEL: default_box_str_len
fn main() {
    assert_eq!(default_box_str_len(), 0);
}
