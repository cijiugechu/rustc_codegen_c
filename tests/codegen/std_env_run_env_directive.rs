//! Verify `//@ run-env: KEY=VALUE` injects env vars for this test process.

//@ run-pass
//@ run-env: RCGENC_RUN_ENV=hello
//@ exit-code: 0

// CHECK-LABEL: main
fn main() {
    match std::env::var("RCGENC_RUN_ENV") {
        Ok(v) if v == "hello" => {}
        _ => std::process::exit(1),
    }
}
