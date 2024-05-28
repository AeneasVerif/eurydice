trait Mul2 {
    fn mul2 (x: Self) -> Self;
    fn add (x: Self, y: Self) -> Self;
}

trait MyImpl<T: Mul2> {
    fn mul4 (x: T, y: T) -> T {
        T::add(T::mul2(x), T::mul2(y))
    }
}

struct Foo { }

impl Mul2 for u64 {
    fn mul2(x: u64) -> u64 { x << 2 }
    fn add(x: u64, y: u64) -> u64 { x + y }
}

impl MyImpl<u64> for Foo {
}

fn main() {
    let x = Foo::mul4(0u64, 0u64);
    assert_eq!(x, 0u64);
}
