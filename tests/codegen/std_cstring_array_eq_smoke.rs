//! Smoke test for CString paths that rely on array equality.

//@ run-pass
//@ exit-code: 0

use std::ffi::CString;

// CHECK-LABEL: main
fn main() {
    let c = CString::new("abc").unwrap();
    let bytes = c.as_bytes_with_nul();
    let lhs = [bytes[0], bytes[1], bytes[2], bytes[3]];
    let rhs = [b'a', b'b', b'c', 0];
    if lhs != rhs {
        std::process::exit(1);
    }

    let rhs_ne = [b'x', b'b', b'c', 0];
    if lhs == rhs_ne {
        std::process::exit(2);
    }

    if CString::new(b"a\0b".to_vec()).is_ok() {
        std::process::exit(3);
    }
}
