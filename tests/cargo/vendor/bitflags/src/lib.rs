pub trait Bits: Copy + Eq {}

macro_rules! impl_bits {
    ($($ty:ty),* $(,)?) => {
        $(impl Bits for $ty {})*
    };
}

impl_bits!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

pub fn assert_bits<T: Bits>() {}
