//! Test global allocator shims with alloc::alloc APIs.

//@ run-pass
//@ exit-code: 0

#![no_std]
#![no_main]

extern crate alloc;

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
}

#[global_allocator]
static GLOBAL: LibcAlloc = LibcAlloc;

unsafe extern "C" {
    fn malloc(size: usize) -> *mut core::ffi::c_void;
    fn free(ptr: *mut core::ffi::c_void);
}

#[panic_handler]
fn panic(_: &PanicInfo<'_>) -> ! {
    loop {}
}

// CHECK: __rust_alloc
// CHECK: __rust_dealloc
#[no_mangle]
pub extern "C" fn main() -> i32 {
    let layout = Layout::new::<u128>();
    unsafe {
        let p = alloc::alloc::alloc(layout);
        if p.is_null() {
            return 1;
        }
        alloc::alloc::dealloc(p, layout);
    }
    0
}
