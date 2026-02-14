//! Test `f32/f64` comparisons and NaN behavior.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_float_cmp_nan.stdout

// CHECK: uint8_t cmp_mask_f32(float _0, float _1);
// CHECK: uint8_t cmp_mask_f64(double _0, double _1);
// CHECK: __builtin_isnan
// CHECK: __builtin_isnan
#[no_mangle]
pub fn cmp_mask_f32(a: f32, b: f32) -> u8 {
    let mut mask = 0u8;
    if a == b {
        mask |= 1;
    }
    if a != b {
        mask |= 2;
    }
    if a < b {
        mask |= 4;
    }
    if a <= b {
        mask |= 8;
    }
    if a > b {
        mask |= 16;
    }
    if a >= b {
        mask |= 32;
    }
    mask
}

#[no_mangle]
pub fn cmp_mask_f64(a: f64, b: f64) -> u8 {
    let mut mask = 0u8;
    if a == b {
        mask |= 1;
    }
    if a != b {
        mask |= 2;
    }
    if a < b {
        mask |= 4;
    }
    if a <= b {
        mask |= 8;
    }
    if a > b {
        mask |= 16;
    }
    if a >= b {
        mask |= 32;
    }
    mask
}

fn main() {
    assert_eq!(cmp_mask_f32(f32::NAN, 1.0), 2);
    assert_eq!(cmp_mask_f32(1.0, 1.0), 41);
    assert_eq!(cmp_mask_f32(2.0, 1.0), 50);

    assert_eq!(cmp_mask_f64(f64::NAN, 1.0), 2);
    assert_eq!(cmp_mask_f64(1.0, 1.0), 41);
    assert_eq!(cmp_mask_f64(2.0, 1.0), 50);

    println!("std_float_cmp_nan: ok");
}
