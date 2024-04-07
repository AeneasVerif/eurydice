fn g<const N: usize, const M: usize>(_x: [u8; N]) -> [u8; M] {
    [0; M]
}

fn f<const N: usize, const M: usize>(x: [u8; N]) -> [u8; M] {
    g::<N,M>(x)
}

fn h<T>(x: T) -> T {
    x
}

fn main() {
    let x = f::<2,4>(h([0; 2]));
    let y = f::<4,2>([0; 4]);
    assert_eq!(x[0], 0);
    assert_eq!(y[0], 0);
}
