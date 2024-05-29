fn test(x: &mut [u8]) -> u8 {
    x[0]
}

fn main() {
    let mut x = [ 0; 8 ];
    let result = test(&mut x[..]);
    let expected = 0;
    assert_eq!(result, expected);
}
