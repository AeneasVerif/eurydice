// The difficulty with implementing traits as dictionaries is the mismatch in terms of const
// generics (types are less of an issue somewhat).

// In this trait decl, method signatures (as found in the trait_decl) are all parametric over K
trait Ops<const K: usize> {
    // usize -> Self -> Self -> Self
    fn add (x: [u16; K], y: Self) -> Self;
    // usize -> u16 -> Self
    fn of_u16 (x: u16) -> Self;
}

// Here, however, the signatures in this trait impl are NOT parametric over K
impl Ops<1> for u64 {
    // [u16; 1] -> u64 -> u64
    fn add (x: [u16; 1], y: u64) -> u64 { Into::<u64>::into(x[0]) + y }
    // u16 -> u64
    fn of_u16 (x: u16) -> u64 { x.into() }
}

impl<const K: usize> Ops<K> for usize {
    // usize -> [u16; 1] -> usize -> usize
    fn add (x: [u16; K], y: usize) -> usize { Into::<usize>::into(x[0]) + y + K }
    // usize -> u16 -> usize
    fn of_u16 (x: u16) -> usize { x.into() }
}

fn fn_k<const K: usize, T: Ops<K>>() -> T {
    // NO CONST GENERICS PROVIDED AT THIS CALL-SITE
    // The call sites only have the the method-level const generics.
    let x = T::of_u16(0);
    T::add([0; K], x)
}

fn fn_1<T: Ops<1>+Copy>() -> T {
    // NO CONST GENERICS PROVIDED AT THIS CALL-SITE
    let x = T::of_u16(0);
    T::add([0; 1], x)
}

fn k_calls_k() {
    // fn_k receives an add function that starts with size_t -> ... (for the impl-level const
    // generic)
    let r = fn_k::<3, usize>();
    let r_expected = 3;
    assert_eq!(r, r_expected);
}

fn k_calls_one() {
    // fn_k receives an add function that DOES NOT start with size_t -> ...
    let r = fn_k::<1, u64>();
    let r_expected = 0;
    assert_eq!(r, r_expected);
}

fn one_calls_k() {
    let r = fn_1::<usize>();
    let r_expected = 1;
    assert_eq!(r, r_expected);
}

fn one_calls_one() {
    let r = fn_1::<u64>();
    let r_expected = 0;
    assert_eq!(r, r_expected);
}

fn double<T: Ops<1> + Copy, U: Ops<1>+Copy> (x: T, y: U) -> (T, U) {
    (T::add([0; 1], x), U::add([0; 1], y))
}

fn double_k<const K: usize, T: Ops<K> + Copy, U: Ops<1>+Copy> (x: T, y: U) -> (T, U) {
    (T::add([0; K], x), U::add([0; 1], y))
}

fn main() {
    // The four common situations with const generics in bounds.
    k_calls_k();
    k_calls_one();
    one_calls_k();
    one_calls_one();

    // Slightly more involved tests.
    let x = double(1u64, 1usize);
    let y = double_k::<3usize, usize, u64>(1usize, 1u64);
    let x_0 = 2u64;
    let x_1 = 3usize;
    assert_eq!(x.0, x_0);
    assert_eq!(x.1, x_1);
    let y_0 = 5usize;
    let y_1 = 2u64;
    assert_eq!(y.0, y_0);
    assert_eq!(y.1, y_1);
}
