//! Test Box and Vec with global allocator shims.

#![no_std]
#![no_main]

extern crate alloc;

use alloc::boxed::Box;
use alloc::vec::Vec;
use core::alloc::{GlobalAlloc, Layout};
use core::panic::PanicInfo;

struct LibcAlloc;

unsafe impl GlobalAlloc for LibcAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        malloc(layout.size()).cast()
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        free(ptr.cast());
    }

    unsafe fn realloc(&self, ptr: *mut u8, _layout: Layout, new_size: usize) -> *mut u8 {
        realloc(ptr.cast(), new_size).cast()
    }
}

#[global_allocator]
static GLOBAL: LibcAlloc = LibcAlloc;

unsafe extern "C" {
    fn malloc(size: usize) -> *mut core::ffi::c_void;
    fn free(ptr: *mut core::ffi::c_void);
    fn realloc(ptr: *mut core::ffi::c_void, size: usize) -> *mut core::ffi::c_void;
}

#[panic_handler]
fn panic(_: &PanicInfo<'_>) -> ! {
    loop {}
}

// CHECK: __rust_alloc
// CHECK: __rust_dealloc
// CHECK: __rust_realloc
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let b = Box::new(41u64);

    let mut v = Vec::with_capacity(1);
    for i in 0..8u64 {
        v.push(i);
    }

    let sum = v.iter().copied().sum::<u64>();
    if *b + sum == 69 {
        0
    } else {
        1
    }
}
