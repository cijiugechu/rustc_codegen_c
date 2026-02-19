//! Smoke test for Rust union lowering semantics in the C backend.

//@ run-pass
//@ exit-code: 0

#[repr(C)]
union Repr {
    word: u32,
    bytes: [u8; 4],
}

// CHECK-LABEL: main
fn main() {
    let mut value = Repr { word: 0x1122_3344 };
    let bytes = unsafe { value.bytes };
    let expected_bytes = if cfg!(target_endian = "little") {
        [0x44, 0x33, 0x22, 0x11]
    } else {
        [0x11, 0x22, 0x33, 0x44]
    };
    if bytes != expected_bytes {
        std::process::exit(1);
    }

    value = Repr { bytes: [0x10, 0x20, 0x30, 0x40] };
    let word = unsafe { value.word };
    let expected_word = if cfg!(target_endian = "little") {
        0x4030_2010
    } else {
        0x1020_3040
    };
    if word != expected_word {
        std::process::exit(2);
    }
}
