//! Regression test for duplicate link symbols across CGUs in TCP-heavy std usage.
//!
//! This is intentionally compile-only (no `//@ run-pass`). The important assertion is
//! that codegen + linking succeeds without duplicate `_rust_eh_personality` or
//! `is_val_statically_known` symbols.

use std::io::ErrorKind;
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::thread;
use std::time::Duration;

fn bind_loopback_listener() -> TcpListener {
    for _ in 0..20 {
        match TcpListener::bind(("127.0.0.1", 0)) {
            Ok(listener) => return listener,
            Err(e) if e.kind() == ErrorKind::AddrInUse => thread::sleep(Duration::from_millis(10)),
            Err(e) => panic!("bind loopback listener failed: {e}"),
        }
    }
    panic!("bind loopback listener failed after retries");
}

fn connect_with_retry(addr: SocketAddr) -> TcpStream {
    for _ in 0..20 {
        match TcpStream::connect(addr) {
            Ok(stream) => return stream,
            Err(e)
                if e.kind() == ErrorKind::ConnectionRefused
                    || e.kind() == ErrorKind::TimedOut
                    || e.kind() == ErrorKind::Interrupted =>
            {
                thread::sleep(Duration::from_millis(10));
            }
            Err(e) => panic!("connect loopback listener failed: {e}"),
        }
    }
    panic!("connect loopback listener failed after retries");
}

// CHECK-LABEL: main
fn main() {
    let listener = bind_loopback_listener();
    let addr = listener.local_addr().expect("read listener addr");
    drop(listener);
    let _ = connect_with_retry(addr);
}
