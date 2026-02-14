#![feature(rustc_private)]
#![no_std]

extern crate libc;

use core::fmt::{self, Write};

static mut PASS_COUNT: i32 = 0;
static mut FAIL_COUNT: i32 = 0;

pub trait HarnessNum: Copy {
    fn trace_value(key: &str, value: Self);
    fn assert_eq_value(desc: &str, expected: Self, actual: Self);
}

struct TraceDebugWriter;

impl Write for TraceDebugWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let (s_ptr, s_len) = str_parts(s);
        unsafe {
            libc::printf(b"%.*s\0".as_ptr().cast(), s_len, s_ptr);
        }
        Ok(())
    }
}

fn record_pass(desc: &str) {
    let (desc_ptr, desc_len) = str_parts(desc);
    unsafe {
        PASS_COUNT += 1;
        libc::printf(b"HARNESS|PASS|%.*s\n\0".as_ptr().cast(), desc_len, desc_ptr);
    }
}

fn record_fail(desc: &str) {
    let (desc_ptr, desc_len) = str_parts(desc);
    unsafe {
        FAIL_COUNT += 1;
        libc::printf(b"HARNESS|FAIL|%.*s\n\0".as_ptr().cast(), desc_len, desc_ptr);
    }
}

fn str_parts(s: &str) -> (*const i8, i32) {
    let len = if s.len() > i32::MAX as usize { i32::MAX } else { s.len() as i32 };
    (s.as_ptr().cast(), len)
}

pub fn begin(case_name: &str) {
    let (case_name_ptr, case_name_len) = str_parts(case_name);
    unsafe {
        PASS_COUNT = 0;
        FAIL_COUNT = 0;
        libc::printf(b"HARNESS|BEGIN|%.*s\n\0".as_ptr().cast(), case_name_len, case_name_ptr);
    }
}

pub fn trace_i64(key: &str, value: i64) {
    let (key_ptr, key_len) = str_parts(key);
    unsafe {
        libc::printf(b"HARNESS|STATE|%.*s|%lld\n\0".as_ptr().cast(), key_len, key_ptr, value);
    }
}

pub fn trace_u64(key: &str, value: u64) {
    let (key_ptr, key_len) = str_parts(key);
    unsafe {
        libc::printf(b"HARNESS|STATE|%.*s|%llu\n\0".as_ptr().cast(), key_len, key_ptr, value);
    }
}

pub fn trace_bool(key: &str, value: bool) {
    let (key_ptr, key_len) = str_parts(key);
    let text = if value { "true" } else { "false" };
    let (value_ptr, value_len) = str_parts(text);
    unsafe {
        libc::printf(
            b"HARNESS|STATE|%.*s|%.*s\n\0".as_ptr().cast(),
            key_len,
            key_ptr,
            value_len,
            value_ptr,
        );
    }
}

pub fn trace_str(key: &str, value: &str) {
    let (key_ptr, key_len) = str_parts(key);
    let (value_ptr, value_len) = str_parts(value);
    unsafe {
        libc::printf(
            b"HARNESS|STATE|%.*s|%.*s\n\0".as_ptr().cast(),
            key_len,
            key_ptr,
            value_len,
            value_ptr,
        );
    }
}

pub fn trace_char(key: &str, value: char) {
    let (key_ptr, key_len) = str_parts(key);
    unsafe {
        libc::printf(
            b"HARNESS|STATE|%.*s|U+%04X\n\0".as_ptr().cast(),
            key_len,
            key_ptr,
            value as u32,
        );
    }
}

pub fn trace_ptr<T>(key: &str, value: *const T) {
    let (key_ptr, key_len) = str_parts(key);
    unsafe {
        libc::printf(
            b"HARNESS|STATE|%.*s|%p\n\0".as_ptr().cast(),
            key_len,
            key_ptr,
            value as *const libc::c_void,
        );
    }
}

pub fn trace_mut_ptr<T>(key: &str, value: *mut T) {
    trace_ptr(key, value as *const T);
}

pub fn assert_eq_i64(desc: &str, expected: i64, actual: i64) {
    let (desc_ptr, desc_len) = str_parts(desc);
    unsafe {
        if expected == actual {
            record_pass(desc);
            return;
        }

        FAIL_COUNT += 1;
        libc::printf(
            b"HARNESS|FAIL|%.*s|expected=%lld|actual=%lld\n\0".as_ptr().cast(),
            desc_len,
            desc_ptr,
            expected,
            actual,
        );
    }
}

pub fn assert_eq_u64(desc: &str, expected: u64, actual: u64) {
    let (desc_ptr, desc_len) = str_parts(desc);
    unsafe {
        if expected == actual {
            record_pass(desc);
            return;
        }

        FAIL_COUNT += 1;
        libc::printf(
            b"HARNESS|FAIL|%.*s|expected=%llu|actual=%llu\n\0".as_ptr().cast(),
            desc_len,
            desc_ptr,
            expected,
            actual,
        );
    }
}

pub fn assert_eq<T: PartialEq>(desc: &str, expected: T, actual: T) {
    if expected == actual {
        record_pass(desc);
    } else {
        record_fail(desc);
    }
}

pub fn trace_num<T: HarnessNum>(key: &str, value: T) {
    T::trace_value(key, value);
}

pub fn assert_eq_num<T: HarnessNum>(desc: &str, expected: T, actual: T) {
    T::assert_eq_value(desc, expected, actual);
}

pub fn trace_value<T: core::fmt::Debug + ?Sized>(key: &str, value: &T) {
    let (key_ptr, key_len) = str_parts(key);
    unsafe {
        libc::printf(b"HARNESS|STATE|%.*s|\0".as_ptr().cast(), key_len, key_ptr);
    }

    let mut writer = TraceDebugWriter;
    let _ = write!(&mut writer, "{:?}", value);

    unsafe {
        libc::printf(b"\n\0".as_ptr().cast());
    }
}

macro_rules! impl_harness_num_signed {
    ($($ty:ty),* $(,)?) => {
        $(
            impl HarnessNum for $ty {
                fn trace_value(key: &str, value: Self) {
                    trace_i64(key, value as i64);
                }

                fn assert_eq_value(desc: &str, expected: Self, actual: Self) {
                    assert_eq_i64(desc, expected as i64, actual as i64);
                }
            }
        )*
    };
}

macro_rules! impl_harness_num_unsigned {
    ($($ty:ty),* $(,)?) => {
        $(
            impl HarnessNum for $ty {
                fn trace_value(key: &str, value: Self) {
                    trace_u64(key, value as u64);
                }

                fn assert_eq_value(desc: &str, expected: Self, actual: Self) {
                    assert_eq_u64(desc, expected as u64, actual as u64);
                }
            }
        )*
    };
}

impl_harness_num_signed!(i8, i16, i32, i64, isize);
impl_harness_num_unsigned!(u8, u16, u32, u64, usize);

#[macro_export]
macro_rules! trace_num {
    ($key:expr, $value:expr $(,)?) => {{
        $crate::trace_num($key, $value)
    }};
}

#[macro_export]
macro_rules! trace {
    ($key:expr, $value:expr $(,)?) => {{
        $crate::trace_value($key, &$value)
    }};
}

#[macro_export]
macro_rules! assert_eq_num {
    ($desc:expr, $expected:expr, $actual:expr $(,)?) => {{
        $crate::assert_eq_num($desc, $expected, $actual)
    }};
}

#[macro_export]
macro_rules! assert_eq {
    ($desc:expr, $expected:expr, $actual:expr $(,)?) => {{
        $crate::assert_eq($desc, $expected, $actual)
    }};
}

pub fn finish() -> i32 {
    unsafe {
        libc::printf(
            b"HARNESS|SUMMARY|pass=%d|fail=%d\n\0".as_ptr().cast(),
            PASS_COUNT,
            FAIL_COUNT,
        );

        if FAIL_COUNT == 0 {
            0
        } else {
            1
        }
    }
}
