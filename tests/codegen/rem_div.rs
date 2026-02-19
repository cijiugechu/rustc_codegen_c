//! Test integer division/remainder lowering for signed and unsigned paths.

//@ run-pass
//@ exit-code: 0

#[no_mangle]
pub fn rem_u32(x: u32, y: u32) -> u32 {
    x % y
}

#[no_mangle]
pub fn rem_i32(x: i32, y: i32) -> i32 {
    x % y
}

#[no_mangle]
pub fn div_u32(x: u32, y: u32) -> u32 {
    x / y
}

#[no_mangle]
pub fn div_i32(x: i32, y: i32) -> i32 {
    x / y
}

// CHECK-DAG: (_0 / _1);
// CHECK-DAG: (_0 / _1);
// CHECK-DAG: (_0 % _1);
// CHECK-DAG: (_0 % _1);
fn main() {
    let a = rem_u32(10, 3);
    let b = rem_i32(10, 3);
    let c = div_u32(9, 2);
    let d = div_i32(-9, 2);

    if a != 1 || b != 1 || c != 4 || d != -4 {
        std::process::abort();
    }
}
