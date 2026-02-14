//! Test `f32/f64` arithmetic codegen paths.

//@ run-pass
//@ exit-code: 0
//@ check-stdout-file: tests/expect/std_float_arith.stdout

const EPS32: f32 = 1e-5;
const EPS64: f64 = 1e-12;

// CHECK: float add_f32(float _0, float _1);
#[no_mangle]
pub fn add_f32(a: f32, b: f32) -> f32 {
    a + b
}

#[no_mangle]
pub fn sub_f32(a: f32, b: f32) -> f32 {
    a - b
}

#[no_mangle]
pub fn mul_f32(a: f32, b: f32) -> f32 {
    a * b
}

#[no_mangle]
pub fn div_f32(a: f32, b: f32) -> f32 {
    a / b
}

// CHECK: double add_f64(double _0, double _1);
// CHECK: __builtin_fmodf
#[no_mangle]
pub fn rem_f32(a: f32, b: f32) -> f32 {
    a % b
}

// CHECK: __builtin_fmod
#[no_mangle]
pub fn rem_f64(a: f64, b: f64) -> f64 {
    a % b
}

#[no_mangle]
pub fn add_f64(a: f64, b: f64) -> f64 {
    a + b
}

#[no_mangle]
pub fn sub_f64(a: f64, b: f64) -> f64 {
    a - b
}

#[no_mangle]
pub fn mul_f64(a: f64, b: f64) -> f64 {
    a * b
}

#[no_mangle]
pub fn div_f64(a: f64, b: f64) -> f64 {
    a / b
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
    let a32 = 7.5f32;
    let b32 = 2.0f32;
    assert!(approx_eq_f32(add_f32(a32, b32), 9.5));
    assert!(approx_eq_f32(sub_f32(a32, b32), 5.5));
    assert!(approx_eq_f32(mul_f32(a32, b32), 15.0));
    assert!(approx_eq_f32(div_f32(a32, b32), 3.75));
    assert!(approx_eq_f32(rem_f32(a32, b32), 1.5));

    let a64 = 9.25f64;
    let b64 = 2.5f64;
    assert!(approx_eq_f64(add_f64(a64, b64), 11.75));
    assert!(approx_eq_f64(sub_f64(a64, b64), 6.75));
    assert!(approx_eq_f64(mul_f64(a64, b64), 23.125));
    assert!(approx_eq_f64(div_f64(a64, b64), 3.7));
    assert!(approx_eq_f64(rem_f64(a64, b64), 1.75));

    println!("std_float_arith: ok");
}
