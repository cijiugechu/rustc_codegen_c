//! Test array variable indexing codegen.

//@ aux-build:mini_core.rs
//@ run-pass
//@ exit-code: 21

#![feature(no_core)]
#![no_core]
#![no_main]

extern crate mini_core;

// CHECK-LABEL: main
// CHECK: _Bool _{{[0-9]+}} = (_{{[0-9]+}} < 3);
// CHECK: __rust_panic_bounds_check
// CHECK: = _{{[0-9]+}}[_{{[0-9]+}}];
// CHECK: (_{{[0-9]+}}[_{{[0-9]+}}] = _{{[0-9]+}});
#[no_mangle]
pub fn main() -> i32 {
    let mut arr: [i32; 3] = [10, 20, 30];

    let mut i: usize = 0;
    i = i + 1;
    let x = arr[i];

    let mut j: usize = i;
    j = j + 1;
    arr[j] = x + 1;

    arr[j]
}
