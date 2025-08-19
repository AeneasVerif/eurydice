trait Foo<const K: usize> {
    fn bar<const L: usize>(x: [[u8; L]; K], y: [[u8; K]; L]) -> Self;
}

impl<const K: usize> Foo<K> for u64 {
    fn bar<const L: usize>(x: [[u8; L]; K], _: [[u8; K]; L]) -> Self {
        x[0][0].into()
    }
}

fn f<const K: usize, const L: usize, const M: usize, T: Foo<L>>() -> T {
    T::bar([[0; 4]; L], [[0; L]; 4])
}

trait UseFoo {
    fn method_foo<const K: usize, T: Foo<K>>() -> T;
}

impl UseFoo for () {
    fn method_foo<const K: usize, T: Foo<K>>() -> T {
        T::bar([[0; 4]; K], [[0; K]; 4])
    }
}

fn g<Scheme: UseFoo>() -> u64 {
    Scheme::method_foo::<12, u64>()
}

fn main() {
    let r = f::<6, 8, 10, u64>();
    assert_eq!(r, 0);

    let r = g::<()>();
    assert_eq!(r, 0);
}
