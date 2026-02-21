//! Regression test for scalar/transmute paths using Result niche layout.

//@ run-pass
//@ exit-code: 0

// CHECK: pack

#[inline(never)]
fn pack(flag: bool, x: usize, p: *const ()) -> Result<usize, *const ()> {
    if flag { Ok(x) } else { Err(p) }
}

#[inline(never)]
fn unpack(v: Result<usize, *const ()>) -> usize {
    match v {
        Ok(x) => x,
        Err(p) => p as usize,
    }
}

fn main() {
    let p = 0x1234usize as *const ();
    if unpack(pack(true, 99, p)) != 99 {
        std::process::exit(1);
    }
    if unpack(pack(false, 99, p)) != p as usize {
        std::process::exit(2);
    }
}
