trait Ops {
    fn add (x: Self, y: Self) -> Self;
    fn of_u32 (x: u32) -> Self;
}

impl Ops for u64 {
    fn add (x: u64, y: u64) -> u64 { x + y }
    fn of_u32 (x: u32) -> u64 { x.into() }
}

impl Ops for u32 {
    fn add (x: u32, y: u32) -> u32 { x + y }
    fn of_u32 (x: u32) -> u32 { x.into() }
}

fn double<T: Ops + Copy, U: Ops+Copy> (x: T, y: U) -> (T, U) {
    (T::add(x, x), U::add(y, y))
}

fn main() {
    let x = double(1u64, 1u32);
    let x_0 = 2u64;
    let x_1 = 2u32;
    assert_eq!(x.0, x_0);
    assert_eq!(x.1, x_1);
}
