fn g<const N: usize, const M: usize>(_x: [u8; N]) -> [u8; M] {
    [0; M]
}

fn f<const N: usize, const M: usize>(x: [u8; N]) -> [u8; M] {
    g::<N,M>(x)
}

fn h<T>(x: T) -> T {
    x
}

type U<const N: usize, T> = ([u8; N], T);

fn i<const N: usize, T>(x: T) -> U<N,T> {
    ([0; N], x)
}

// Goal: trigger a monomorphization that can only be observed via symbolic evaluation of const
// generics.

type T<const N:usize> = ([u8; N], [u8; N]);

fn j<const N:usize>() -> usize {
    let tmp: T::<N> = ([0u8; N], [0u8; N]);
    tmp.0.len() + tmp.1.len()
}

fn j_api() {
    let l = j::<16>();
    assert_eq!(l, 32);
}

fn main() {
    let x = f::<2,4>(h([0; 2]));
    let y = f::<4,2>([0; 4]);
    let z = i::<1,[u8; 1]>([0; 1]);
    j_api();
    assert_eq!(x[0], 0);
    assert_eq!(y[0], 0);
    assert_eq!(z.1[0], 0);
}
