//! End-to-end smoke test for i128/u128 lowering through native __int128 paths.

//@ run-pass
//@ exit-code: 0

// CHECK: #if !defined(__SIZEOF_INT128__)
// CHECK: #error "rustc_codegen_c requires __int128 support when lowering i128/u128"
// CHECK: typedef __int128 __rcgenc_i128;
// CHECK: typedef unsigned __int128 __rcgenc_u128;
// CHECK: __rust_u128_from_parts
// CHECK: __rust_popcount_u128

const BIG_I: i128 = (1i128 << 100) + 12345;
const BIG_U: u128 = (1u128 << 120) | 0x1234_5678_9ABC_DEF0u128;

#[inline(never)]
fn classify_u128(v: u128) -> u8 {
    match v {
        BIG_U => 7,
        _ => 1,
    }
}

#[inline(never)]
fn consume_i128(v: i128) -> i128 {
    std::hint::black_box(v)
}

#[inline(never)]
fn consume_u128(v: u128) -> u128 {
    std::hint::black_box(v)
}

fn main() {
    let a = consume_i128(BIG_I);
    let b = consume_i128((1i128 << 96) - 77);
    let ua = consume_u128(BIG_U);

    let mix_i = (a + b - b) ^ (b * 3 / 3) ^ (b * 3 % 7);
    let mix_u = ((ua << 7) >> 7) ^ (ua | 0x55) ^ (ua & 0x33);

    let cmp_a = (a > b) as u128;
    let cmp_b = (ua >= (1u128 << 64)) as u128;
    let cast_a = (((1u128 << 64) + 99) as i64) as u128;
    let cast_b = ((-1i128) as u128) >> 127;
    let ones_u = ua.count_ones() as u128;
    let ones_i = consume_i128(BIG_I).count_ones() as u128;
    let class_a = classify_u128(consume_u128(BIG_U)) as u128;
    let class_b = classify_u128(consume_u128(BIG_U - 1)) as u128;

    let checksum = (mix_i as u128)
        ^ mix_u
        ^ cmp_a
        ^ cmp_b
        ^ cast_a
        ^ cast_b
        ^ ones_u
        ^ ones_i
        ^ (class_a << 8)
        ^ class_b;
    if checksum == 0 {
        std::process::abort();
    }
}
