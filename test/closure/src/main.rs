fn f() -> [usize; 1] {
    let s = [0; 1];
    let a: [usize; 1] = core::array::from_fn(|i| s[0]+i);
    a
}

fn main() {
    let actual = f()[0];
    let expected = 0;
    assert_eq!(actual, expected);
}
