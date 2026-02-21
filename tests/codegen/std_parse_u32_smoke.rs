//! Regression test for PassMode::Cast single-piece return handling.

//@ run-pass
//@ exit-code: 0

#[inline(never)]
#[no_mangle]
pub fn parse_decimal(input: &str) -> u32 {
    match input.parse::<u32>() {
        Ok(v) => v,
        Err(_) => 0,
    }
}

// CHECK-LABEL: parse_decimal
fn main() {
    assert_eq!(parse_decimal("123"), 123);
    assert_eq!(parse_decimal("nope"), 0);
}
