pub fn f(a: usize, b: usize) -> usize {
    a.min(b)
}

fn main() {
    let expected = 0;
    let actual = f(0, 0);
    assert_eq!(expected, actual);
}
