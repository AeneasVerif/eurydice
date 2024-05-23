trait Foo<const K: usize> {
    fn bar<const L: usize>(x: [[u8; L]; K], y: [[u8; K]; L]) -> Self;
}

impl<const K: usize> Foo<K> for u64 {
    fn bar<const L: usize>(x: [[u8; L]; K], _: [[u8; K]; L]) -> Self { x[0][0].into() }
}

fn f<const K: usize, T: Foo<K>>() -> T {
    T::bar([[0; 4]; K], [[0; K]; 4])
}

fn main() {
    let r = f::<8, u64>();
    let expected = 0;
    assert_eq!(r, expected);
}
