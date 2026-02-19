//! Ensure signed integer immediates preserve sign when lowered.

//@ run-pass
//@ exit-code: 0

#[no_mangle]
pub fn is_lt_neg_four(x: i32) -> bool {
    x < -4
}

// CHECK-LABEL: main
fn main() {
    if is_lt_neg_four(0) || !is_lt_neg_four(-5) {
        std::process::abort();
    }
}
