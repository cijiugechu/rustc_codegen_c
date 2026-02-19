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
