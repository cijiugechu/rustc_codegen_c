//! Regression test for write_operand_repeatedly lowering used by repeat array init.

//@ run-pass
//@ exit-code: 0

// CHECK: repeat_loop_header

#[inline(never)]
fn repeated() -> [u16; 8] {
    [0x1234u16; 8]
}

fn main() {
    let arr = repeated();
    for value in arr {
        if value != 0x1234 {
            std::process::exit(1);
        }
    }
}
