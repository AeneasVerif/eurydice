fn f() -> [[usize; 1]; 1] {
    let s = [0; 1];
    let a: [[usize; 1]; 1] = core::array::from_fn(|i| {
        core::array::from_fn(|j| (s[0]+i+j))
    });
    a
}

fn main() {
    let actual = f()[0][0];
    let expected = 0;
    assert_eq!(actual, expected);
}
