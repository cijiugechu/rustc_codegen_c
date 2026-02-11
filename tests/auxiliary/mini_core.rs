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

#[lang = "add"]
pub trait Add<Rhs = Self> {
    type Output;
    fn add(self, rhs: Rhs) -> Self::Output;
}

impl Copy for bool {}
impl Copy for u8 {}
impl Copy for u16 {}
impl Copy for u32 {}
impl Copy for u64 {}
impl Copy for usize {}
impl Copy for i8 {}
impl Copy for i16 {}
impl Copy for i32 {}
impl Copy for isize {}
impl Copy for f32 {}
impl Copy for f64 {}
impl Copy for char {}
impl<'a, T: ?Sized> Copy for &'a T {}
impl<T: ?Sized> Copy for *const T {}
impl<T: ?Sized> Copy for *mut T {}

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
