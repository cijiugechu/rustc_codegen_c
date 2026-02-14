//! Test common `std::ffi::CStr` usage paths.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_cstr.stdout

use std::ffi::{c_char, CStr};

// CHECK-LABEL: main
fn bytes_match(actual: &[u8], expected: &[u8]) -> bool {
    if actual.len() != expected.len() {
        return false;
    }
    let mut i = 0usize;
    while i < actual.len() {
        if actual[i] != expected[i] {
            return false;
        }
        i += 1;
    }
    true
}

fn main() {
    let s1 = CStr::from_bytes_with_nul(b"hello\0").unwrap();
    assert!(bytes_match(s1.to_bytes(), b"hello"));
    assert!(bytes_match(s1.to_bytes_with_nul(), b"hello\0"));
    assert!(bytes_match(s1.to_str().unwrap().as_bytes(), b"hello"));

    let s2 = CStr::from_bytes_until_nul(b"world\0tail").unwrap();
    assert!(bytes_match(s2.to_bytes(), b"world"));
    assert!(bytes_match(s2.to_str().unwrap().as_bytes(), b"world"));

    let raw = b"probe\0".as_ptr() as *const c_char;
    let s3 = unsafe { CStr::from_ptr(raw) };
    assert!(bytes_match(s3.to_bytes(), b"probe"));
    assert!(bytes_match(s3.to_str().unwrap().as_bytes(), b"probe"));

    let empty = CStr::from_bytes_with_nul(b"\0").unwrap();
    assert!(bytes_match(empty.to_bytes(), b""));
    assert!(bytes_match(empty.to_bytes_with_nul(), b"\0"));

    assert!(CStr::from_bytes_with_nul(b"no-nul").is_err());
    assert!(CStr::from_bytes_with_nul(b"a\0b\0").is_err());
    assert!(CStr::from_bytes_until_nul(b"still-no-nul").is_err());

    println!("std_cstr: ok");
}
