fn f(x: &mut [u8]) {}

fn main() {
    let mut x = [0; 1];
    f(&mut x[..]);
}
