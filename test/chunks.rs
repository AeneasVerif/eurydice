fn main() {
    let a = [ 0, 1, 2, 3, 4, 5, 6 ];
    let mut i = 0;
    // This one not supported -- all the other ones are.
    for v in a {
        i += v;
    }
    for v in a.iter() {
        i += v;
    }
    for _ in a.chunks(2) {
        i += 1;
    }
    for _ in a.chunks_exact(2) {
        i += 1;
    }
    let expected = 28;
    assert_eq!(i, expected);
}
