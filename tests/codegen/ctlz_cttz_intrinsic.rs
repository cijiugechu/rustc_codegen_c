//! Smoke test for ctlz/cttz intrinsic lowering, including *_nonzero and 128-bit paths.

//@ run-pass
//@ exit-code: 0

#![feature(core_intrinsics)]
#![allow(internal_features)]

use core::intrinsics;

// CHECK-DAG: __builtin_ctz
// CHECK-DAG: __builtin_ctzll
// CHECK-DAG: __builtin_clz
// CHECK-DAG: __builtin_clzll
// CHECK-DAG: __rust_ctz_u128
// CHECK-DAG: __rust_clz_u128
// CHECK-DAG: __rust_ctz_nonzero_u128
// CHECK-DAG: __rust_clz_nonzero_u128

const WIDE_U64: u64 = 1u64 << 40;
const WIDE_U128_TZ: u128 = 1u128 << 100;
const WIDE_U128_LZ: u128 = 1u128 << 80;
const NZ_U128_TZ: u128 = 1u128 << 127;

#[inline(never)]
fn cttz_u16(v: u16) -> u32 {
    intrinsics::cttz(v)
}

#[inline(never)]
fn ctlz_u16(v: u16) -> u32 {
    intrinsics::ctlz(v)
}

#[inline(never)]
fn ctlz_u8(v: u8) -> u32 {
    intrinsics::ctlz(v)
}

#[inline(never)]
fn cttz_u64(v: u64) -> u32 {
    intrinsics::cttz(v)
}

#[inline(never)]
fn ctlz_u64(v: u64) -> u32 {
    intrinsics::ctlz(v)
}

#[inline(never)]
unsafe fn cttz_nonzero_u8(v: u8) -> u32 {
    unsafe { intrinsics::cttz_nonzero(v) }
}

#[inline(never)]
unsafe fn ctlz_nonzero_u8(v: u8) -> u32 {
    unsafe { intrinsics::ctlz_nonzero(v) }
}

#[inline(never)]
fn cttz_u128(v: u128) -> u32 {
    intrinsics::cttz(v)
}

#[inline(never)]
fn ctlz_u128(v: u128) -> u32 {
    intrinsics::ctlz(v)
}

#[inline(never)]
unsafe fn cttz_nonzero_u128(v: u128) -> u32 {
    unsafe { intrinsics::cttz_nonzero(v) }
}

#[inline(never)]
unsafe fn ctlz_nonzero_u128(v: u128) -> u32 {
    unsafe { intrinsics::ctlz_nonzero(v) }
}

#[inline(never)]
fn cttz_i128(v: i128) -> u32 {
    intrinsics::cttz(v)
}

#[inline(never)]
fn ctlz_i128(v: i128) -> u32 {
    intrinsics::ctlz(v)
}

fn main() {
    let z16 = std::hint::black_box(0u16);
    if cttz_u16(z16) != 16 {
        std::process::exit(1);
    }
    if ctlz_u16(z16) != 16 {
        std::process::exit(2);
    }

    let narrow = std::hint::black_box(0b0001_0000u8);
    if ctlz_u8(narrow) != 3 {
        std::process::exit(3);
    }

    let wide = std::hint::black_box(WIDE_U64);
    if cttz_u64(wide) != 40 {
        std::process::exit(4);
    }
    if ctlz_u64(wide) != 23 {
        std::process::exit(5);
    }

    let nz_cttz_u8 = std::hint::black_box(0b0011_1000u8);
    if unsafe { cttz_nonzero_u8(nz_cttz_u8) } != 3 {
        std::process::exit(6);
    }
    let nz_ctlz_u8 = std::hint::black_box(0b0001_1100u8);
    if unsafe { ctlz_nonzero_u8(nz_ctlz_u8) } != 3 {
        std::process::exit(7);
    }

    let wide_u128_tz = std::hint::black_box(WIDE_U128_TZ);
    if cttz_u128(wide_u128_tz) != 100 {
        std::process::exit(8);
    }
    let wide_u128_lz = std::hint::black_box(WIDE_U128_LZ);
    if ctlz_u128(wide_u128_lz) != 47 {
        std::process::exit(9);
    }

    let signed = std::hint::black_box(i128::MIN);
    if cttz_i128(signed) != 127 {
        std::process::exit(10);
    }
    if ctlz_i128(signed) != 0 {
        std::process::exit(11);
    }

    let nz_cttz_u128 = std::hint::black_box(NZ_U128_TZ);
    if unsafe { cttz_nonzero_u128(nz_cttz_u128) } != 127 {
        std::process::exit(12);
    }
    let nz_ctlz_u128 = std::hint::black_box(1u128);
    if unsafe { ctlz_nonzero_u128(nz_ctlz_u128) } != 127 {
        std::process::exit(13);
    }
}
