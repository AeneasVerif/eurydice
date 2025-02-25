pub trait Operations: Clone + Copy {
    fn zero() -> Self;
}
pub struct MlKemKeyPairUnpacked<const K: usize, Vector: Operations> {
    x: [Vector; K],
}

impl<const K: usize, Vector: Operations> MlKemKeyPairUnpacked<K, Vector> {
    #[inline(always)]
    pub(crate) fn default() -> Self {
        Self {
            x: core::array::from_fn(|_| Vector::zero()),
        }
    }
}

fn main() {
}
