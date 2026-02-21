// file: basic_math.7206e0a37c6be10d-cgu.0.c
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

/* Some helper macros for the generated code */

/** Casts an unsigned integer to a signed integer of the same size.
  * This is used to avoid UB when do integer casting in Rust.
  *
  * The parameter `u` is the unsigned type, `s` is the signed type,
  * `v` is the value to cast, and `m` is the maximum value of the signed type.\
  *
  * example: `__rust_utos(uint32_t, int32_t, x, INT32_MAX)`
  */
#define __rust_utos(u, s, v, m) \
    ((v) <= (m) ? ((s)v) : ((s)((u)(v) - (u)(m) - 1)))

#if defined(__SIZEOF_INT128__)
typedef __int128 __rcgenc_i128;
typedef unsigned __int128 __rcgenc_u128;

#define __RUST_U128_MAX \
    ((((unsigned __int128)0xFFFFFFFFFFFFFFFFULL) << 64) | (unsigned __int128)0xFFFFFFFFFFFFFFFFULL)
#define __RUST_I128_MAX \
    ((__rcgenc_i128)((((unsigned __int128)0x7FFFFFFFFFFFFFFFULL) << 64) | (unsigned __int128)0xFFFFFFFFFFFFFFFFULL))
#define __rust_u128_from_parts(hi, lo) \
    ((((unsigned __int128)(uint64_t)(hi)) << 64) | ((unsigned __int128)(uint64_t)(lo)))
#define __rust_i128_from_parts(hi, lo) ((__rcgenc_i128)__rust_u128_from_parts((hi), (lo)))

static inline unsigned int __rust_popcount_u128(unsigned __int128 value) {
    uint64_t lo = (uint64_t)value;
    uint64_t hi = (uint64_t)(value >> 64);
    return (unsigned int)(__builtin_popcountll(lo) + __builtin_popcountll(hi));
}
#endif

static inline void __rust_black_box_observe(const void *ptr, size_t size) {
    if (size != 0) {
        (void)*(const volatile unsigned char *)ptr;
    }
}

static inline _Noreturn void __rust_panic_bounds_check(size_t index, size_t len) {
    (void)index;
    (void)len;
    abort();
}

/* Helpers for building `_Atomic(T) *` casts from generated code. */
#define __rust_atomic_bool_ptr(p) ((_Atomic(_Bool) *)(p))
#define __rust_atomic_bool_const_ptr(p) ((_Atomic(_Bool) const *)(p))
#define __rust_atomic_i8_ptr(p) ((_Atomic(int8_t) *)(p))
#define __rust_atomic_i8_const_ptr(p) ((_Atomic(int8_t) const *)(p))
#define __rust_atomic_i16_ptr(p) ((_Atomic(int16_t) *)(p))
#define __rust_atomic_i16_const_ptr(p) ((_Atomic(int16_t) const *)(p))
#define __rust_atomic_i32_ptr(p) ((_Atomic(int32_t) *)(p))
#define __rust_atomic_i32_const_ptr(p) ((_Atomic(int32_t) const *)(p))
#define __rust_atomic_i64_ptr(p) ((_Atomic(int64_t) *)(p))
#define __rust_atomic_i64_const_ptr(p) ((_Atomic(int64_t) const *)(p))
#define __rust_atomic_u8_ptr(p) ((_Atomic(uint8_t) *)(p))
#define __rust_atomic_u8_const_ptr(p) ((_Atomic(uint8_t) const *)(p))
#define __rust_atomic_u16_ptr(p) ((_Atomic(uint16_t) *)(p))
#define __rust_atomic_u16_const_ptr(p) ((_Atomic(uint16_t) const *)(p))
#define __rust_atomic_u32_ptr(p) ((_Atomic(uint32_t) *)(p))
#define __rust_atomic_u32_const_ptr(p) ((_Atomic(uint32_t) const *)(p))
#define __rust_atomic_u64_ptr(p) ((_Atomic(uint64_t) *)(p))
#define __rust_atomic_u64_const_ptr(p) ((_Atomic(uint64_t) const *)(p))
#define __rust_atomic_size_ptr(p) ((_Atomic(size_t) *)(p))
#define __rust_atomic_size_const_ptr(p) ((_Atomic(size_t) const *)(p))

int64_t foo(uint8_t _0, uint16_t _1, uint32_t _2);
int32_t main();

int64_t foo(uint8_t _0, uint16_t _1, uint32_t _2)
{
  bb0_start:
  int64_t _3 = __rust_utos(uint64_t, int64_t, (int64_t) _0, INT64_MAX);
  return _3;
}

int32_t main()
{
  bb0_start:
  return 0;
}
