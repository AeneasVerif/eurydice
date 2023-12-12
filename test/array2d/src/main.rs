fn f(mut x: [[u32; 2]; 4]) -> bool {
    x[0] = [ 1, 2 ];
    let y: [[u32; 2]; 4] = [
        [1, 2],
        [3, 4],
        [1, 2],
        [3, 4],
    ];
    x == y
}

fn main() {
    let mut y = [[1,2]; 4];
    y[1] = [3, 4];
    y[3] = [3, 4];
    let actual = f(y);
    let expected = true;
    assert_eq!(actual, expected);
}
