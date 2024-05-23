trait Foo<const K: usize> {
    fn bar<const L: usize>(x: [[u8; L]; K], y: [[u8; K]; L]) -> Self;
}

impl<const K: usize> Foo<K> for u64 {
    fn bar<const L: usize>(x: [[u8; L]; K], _: [[u8; K]; L]) -> Self { x[0][0].into() }
}

fn f<const K: usize, const L: usize, const M:usize, T: Foo<L>>() -> T {
    T::bar([[0; 4]; L], [[0; L]; 4])
}

fn main() {
    let r = f::<6, 8, 10, u64>();
    let expected = 0;
    assert_eq!(r, expected);
}
