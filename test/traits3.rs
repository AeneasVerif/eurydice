pub trait KeccakStateItem<const N: usize>: internal::KeccakItem<N> {}

impl<const N: usize, T: internal::KeccakItem<N>> KeccakStateItem<N> for T {}

pub(crate) mod internal {
    /// A trait for multiplexing implementations.
    pub trait KeccakItem<const N: usize>: Clone + Copy {
        fn zero() -> Self;
    }
}

pub(crate) fn keccak<const N: usize, T: KeccakStateItem<N>>(
) {
}

type uint64x2_t = (u64,u64);

impl internal::KeccakItem<2> for uint64x2_t {
    fn zero() -> Self {
        (0, 0)
    }
}

fn keccakx2() {
    keccak::<2, uint64x2_t>()
}

fn main() {}
