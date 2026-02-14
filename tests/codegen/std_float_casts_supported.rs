//! Test supported float casts: int->float and f32<->f64.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_float_casts_supported.stdout

const EPS32: f32 = 1e-5;
const EPS64: f64 = 1e-12;

// CHECK: double cast_f32_to_f64(float _0);
// CHECK: float cast_f64_to_f32(double _0);
// CHECK: double cast_i64_to_f64(int64_t _0);
// CHECK: float cast_u32_to_f32(uint32_t _0);
// CHECK: = (double) _0;
// CHECK: = (float) _0;
#[no_mangle]
pub fn cast_u32_to_f32(v: u32) -> f32 {
    v as f32
}

#[no_mangle]
pub fn cast_i64_to_f64(v: i64) -> f64 {
    v as f64
}

#[no_mangle]
pub fn cast_f32_to_f64(v: f32) -> f64 {
    v as f64
}

#[no_mangle]
pub fn cast_f64_to_f32(v: f64) -> f32 {
    v as f32
}

fn approx_eq_f32(a: f32, b: f32) -> bool {
    let d = if a >= b { a - b } else { b - a };
    d < EPS32
}

fn approx_eq_f64(a: f64, b: f64) -> bool {
    let d = if a >= b { a - b } else { b - a };
    d < EPS64
}

fn main() {
    assert!(approx_eq_f32(cast_u32_to_f32(42), 42.0));
    assert!(approx_eq_f64(cast_i64_to_f64(-7), -7.0));
    assert!(approx_eq_f64(cast_f32_to_f64(3.5), 3.5));
    assert!(approx_eq_f32(cast_f64_to_f32(9.25), 9.25));

    println!("std_float_casts_supported: ok");
}
