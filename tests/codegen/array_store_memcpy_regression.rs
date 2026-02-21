//! Ensure array stores lower through memcpy instead of invalid C array assignment.

//@ run-pass
//@ exit-code: 66

#[inline(never)]
unsafe fn copy_array(dst: *mut [u64; 3], src: [u64; 3]) {
    *dst = src;
}

// CHECK-LABEL: copy_array
// CHECK: __builtin_memcpy
// CHECK-NOT: (*(uint64_t (*)[3]) _{{[0-9]+}} = _{{[0-9]+}});
fn main() {
    let mut out = [0u64; 3];
    unsafe {
        copy_array(&mut out, [11, 22, 33]);
    }
    std::process::exit((out[0] + out[1] + out[2]) as i32);
}
