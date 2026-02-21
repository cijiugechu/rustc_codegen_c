fn main() {
    bitflags::assert_bits::<u128>();
    bitflags::assert_bits::<i128>();

    let value = (1u128 << 100) | 7;
    if value.count_ones() != 4 {
        std::process::abort();
    }
}
