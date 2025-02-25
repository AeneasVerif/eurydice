trait Ops<const K: usize> {
    fn zero() -> Self;
    fn of_usize(x: usize) -> Self;
}

impl Ops<1> for usize {
    fn zero() -> Self { 0 }
    fn of_usize(x: usize) -> Self { x.into() }
}

fn test<const K:usize, T: Ops<K>+Copy>() -> (T, T) {
    let x: [ T; 1 ] = core::array::from_fn(|i| T::of_usize(i));
    let y = T::zero();
    (x[0], y)
}

fn main() { let (x, y) = test::<1,usize>(); assert_eq!(x, y); }
