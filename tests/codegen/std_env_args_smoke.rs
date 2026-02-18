//! Smoke test for std::env::args codegen path.

//@ run-pass
//@ exit-code: 0

// CHECK-LABEL: main
fn main() {
    let mut args = std::env::args();
    let prog = args.next().unwrap_or_default();
    if prog.is_empty() {
        std::process::exit(1);
    }
}
