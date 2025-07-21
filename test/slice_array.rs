fn f1() {
    let mut x = [[0u8; 4]; 4];
    let (y0, _y1) = x.split_at_mut(2);
    y0[0][0] = 1;
    let actual = x[0][0];
    let expected = 1;
    assert_eq!(actual, expected);
}

fn f2() {
    let mut x = [[0u8; 4]; 4];
    let (y0, _y1) = x.split_at_mut(2);
    // generates a copy
    let mut z = y0[0];
    z[0] = 1;
    let actual = x[0][0];
    let expected = 0;
    assert_eq!(actual, expected);
    assert_eq!(z[0],1);
}

fn main() {
    f1();
    f2();
}
