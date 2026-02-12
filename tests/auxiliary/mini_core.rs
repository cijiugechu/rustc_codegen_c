#![feature(no_core, lang_items, rustc_attrs, intrinsics, decl_macro)]
#![no_core]
#![allow(internal_features)]

#[lang = "pointee_sized"]
pub trait PointeeSized {}

#[lang = "meta_sized"]
pub trait MetaSized: PointeeSized {}

#[lang = "sized"]
pub trait Sized: MetaSized {}

#[lang = "copy"]
pub trait Copy {}

#[lang = "sync"]
pub unsafe trait Sync {}

#[lang = "add"]
pub trait Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}

#[lang = "sub"]
pub trait Sub<Rhs = Self> {
    type Output;
    fn sub(self, rhs: Rhs) -> Self::Output;
}

#[lang = "mul"]
pub trait Mul<Rhs = Self> {
    type Output;
    fn mul(self, rhs: Rhs) -> Self::Output;
}

#[lang = "index"]
pub trait Index<Idx> {
    type Output: ?Sized;
    fn index(&self, index: Idx) -> &Self::Output;
}

#[lang = "index_mut"]
pub trait IndexMut<Idx>: Index<Idx> {
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output;
}

#[lang = "legacy_receiver"]
pub trait LegacyReceiver {}

impl<T: ?Sized> LegacyReceiver for &T {}
impl<T: ?Sized> LegacyReceiver for &mut T {}

impl Copy for bool {}
impl Copy for u8 {}
impl Copy for u16 {}
impl Copy for u32 {}
impl Copy for u64 {}
impl Copy for usize {}
impl Copy for i8 {}
impl Copy for i16 {}
impl Copy for i32 {}
impl Copy for i64 {}
impl Copy for isize {}
impl Copy for f32 {}
impl Copy for f64 {}
impl Copy for char {}
impl<'a, T: ?Sized> Copy for &'a T {}
impl<T: ?Sized> Copy for *const T {}
impl<T: ?Sized> Copy for *mut T {}

unsafe impl<T: ?Sized> Sync for &T {}
unsafe impl<T: ?Sized> Sync for *const T {}
unsafe impl<T: ?Sized> Sync for *mut T {}
unsafe impl Sync for bool {}
unsafe impl Sync for u8 {}
unsafe impl Sync for u16 {}
unsafe impl Sync for u32 {}
unsafe impl Sync for u64 {}
unsafe impl Sync for usize {}
unsafe impl Sync for i8 {}
unsafe impl Sync for i16 {}
unsafe impl Sync for i32 {}
unsafe impl Sync for i64 {}
unsafe impl Sync for isize {}
unsafe impl Sync for f32 {}
unsafe impl Sync for f64 {}
unsafe impl Sync for char {}
unsafe impl Sync for str {}

macro_rules! impl_add_for_int {
    ($($ty:ty),* $(,)?) => {
        $(
            impl Add for $ty {
                type Output = $ty;

                fn add(self, rhs: $ty) -> $ty {
                    self + rhs
                }
            }
        )*
    };
}

impl_add_for_int!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);

macro_rules! impl_sub_for_int {
    ($($ty:ty),* $(,)?) => {
        $(
            impl Sub for $ty {
                type Output = $ty;

                fn sub(self, rhs: $ty) -> $ty {
                    self - rhs
                }
            }
        )*
    };
}

impl_sub_for_int!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);

macro_rules! impl_mul_for_int {
    ($($ty:ty),* $(,)?) => {
        $(
            impl Mul for $ty {
                type Output = $ty;

                fn mul(self, rhs: $ty) -> $ty {
                    self * rhs
                }
            }
        )*
    };
}

impl_mul_for_int!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);

impl<T, const N: usize> Index<usize> for [T; N] {
    type Output = T;

    fn index(&self, _index: usize) -> &T {
        // This test mini_core only provides the lang items needed by type checking.
        // Actual array indexing semantics are lowered directly by rustc_codegen_c,
        // so these trait methods are intentionally never used at runtime.
        loop {}
    }
}

impl<T, const N: usize> IndexMut<usize> for [T; N] {
    fn index_mut(&mut self, _index: usize) -> &mut T {
        // See the note in Index::index above.
        loop {}
    }
}

#[lang = "panic_bounds_check"]
#[track_caller]
fn panic_bounds_check(index: usize, len: usize) -> ! {
    unsafe { __rust_panic_bounds_check(index, len) }
}

#[lang = "panic_location"]
struct PanicLocation {
    file: &'static str,
    line: u32,
    column: u32,
}

#[lang = "drop_in_place"]
unsafe fn drop_in_place<T: ?Sized>(_to_drop: *mut T) {}

unsafe extern "C" {
    fn __rust_panic_bounds_check(index: usize, len: usize) -> !;
}

pub mod libc {
    #[link(name = "c")]
    unsafe extern "C" {
        pub fn puts(s: *const u8) -> i32;
        pub fn printf(format: *const i8, ...) -> i32;
        pub fn malloc(size: usize) -> *mut u8;
        pub fn free(ptr: *mut u8);
        pub fn memcpy(dst: *mut u8, src: *const u8, size: usize);
        pub fn memmove(dst: *mut u8, src: *const u8, size: usize);
        pub fn strncpy(dst: *mut u8, src: *const u8, size: usize);
    }
}
