fn mut_array(mut x: [u32; 1]) {
    x[0] = 1u32;
}

fn main() {
    let x = [0u32];
    let expected = 0u32;
    mut_array(x);
    assert_eq!(x[0], expected);
}
