fn main() {
    let x = -1i16 << 8;
    assert_eq!(x, -256i16);

    let y = ((-1i16 & 0xff00u16 as i16) << 8) >> 8;
    assert_eq!(y, 0);
}
