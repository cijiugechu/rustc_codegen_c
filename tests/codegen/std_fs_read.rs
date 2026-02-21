//! Smoke test for std::fs::read with a repository fixture.

//@ run-pass
//@ check-stdout-file: tests/fixtures/std_fs_read_fixture.txt

use std::fs;

const FIXTURE_PATH: &str = "tests/fixtures/std_fs_read_fixture.txt";
const EXPECTED: &[u8] = include_bytes!("../fixtures/std_fs_read_fixture.txt");

// CHECK-LABEL: main
fn main() {
    let bytes = fs::read(FIXTURE_PATH).expect("fixture read should succeed");
    assert_eq!(bytes, EXPECTED);

    let text = std::str::from_utf8(&bytes).expect("fixture should be utf-8");
    print!("{text}");
}
