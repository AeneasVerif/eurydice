fn b(x: &[u8]) -> [u8; 32] {
    [0u8; 32]
}

pub fn bb(x: &[u8]) -> [u8; 32] {
    core::hint::black_box(b(x))
}

fn main() {
    println!("Hello, world!");
}
