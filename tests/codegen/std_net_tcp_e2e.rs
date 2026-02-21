//! End-to-end smoke test for std::net TCP listener/stream APIs.

//@ run-pass
//@ check-stdout-file: tests/expect/std_net_tcp_e2e.stdout

use std::io::{Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::thread;
use std::time::Duration;

// CHECK-LABEL: main
fn main() {
    // Bind to port 0 so the kernel always picks a free ephemeral local port.
    let listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind loopback listener");
    let addr = listener.local_addr().expect("read listener addr");

    let server = thread::spawn(move || {
        let (mut conn, _) = listener.accept().expect("accept one client");
        conn.set_read_timeout(Some(Duration::from_secs(2))).expect("set server read timeout");
        conn.set_write_timeout(Some(Duration::from_secs(2))).expect("set server write timeout");

        let mut req = [0u8; 4];
        conn.read_exact(&mut req).expect("read client payload");
        if req[0] != b'p' || req[1] != b'i' || req[2] != b'n' || req[3] != b'g' {
            std::process::exit(21);
        }

        conn.write_all(b"pong").expect("write server response");
        conn.flush().expect("flush server response");
        conn.shutdown(Shutdown::Write).expect("shutdown server write side");
    });

    let mut client = TcpStream::connect(addr).expect("connect loopback listener");
    client.set_read_timeout(Some(Duration::from_secs(2))).expect("set client read timeout");
    client.set_write_timeout(Some(Duration::from_secs(2))).expect("set client write timeout");

    client.write_all(b"ping").expect("write client payload");
    client.flush().expect("flush client payload");
    client.shutdown(Shutdown::Write).expect("shutdown client write side");

    let mut res = [0u8; 4];
    client.read_exact(&mut res).expect("read server response");
    if res[0] != b'p' || res[1] != b'o' || res[2] != b'n' || res[3] != b'g' {
        std::process::exit(22);
    }

    server.join().expect("join server thread");
    println!("tcp e2e ok");
}
