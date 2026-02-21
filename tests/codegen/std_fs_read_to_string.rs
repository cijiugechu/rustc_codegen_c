//! Smoke test for std::fs::read_to_string with a repository fixture.

//@ run-pass
//@ check-stdout-file: tests/fixtures/std_fs_read_fixture.txt

use std::fs;

const FIXTURE_PATH: &str = "tests/fixtures/std_fs_read_fixture.txt";
const EXPECTED: &str = include_str!("../fixtures/std_fs_read_fixture.txt");

// CHECK-LABEL: main
fn main() {
    let text = fs::read_to_string(FIXTURE_PATH).expect("fixture read_to_string should succeed");
    assert_eq!(text, EXPECTED);
    print!("{text}");
}
