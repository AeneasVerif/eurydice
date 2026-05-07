fn main() {
    let x = -1i32;
    let y = x << 1;
    assert_eq!(y, -2);

    let x = -1i8;
    let y = x << 1;
    assert_eq!(y, -2);

    let x = -1i16;
    let y = x << 1;
    assert_eq!(y, -2);

    let x = -1i64;
    let y = x << 1;
    assert_eq!(y, -2);
}
