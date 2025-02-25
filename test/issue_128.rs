enum E {
    A = 1,
    B = 2,
}

fn fun_a(_x: &[u8]) {}
fn fun_b(_x: &[u8]) {}

fn use_enum(e: E, x: &[u8]) {
    match e {
        E::A => fun_a(x),
        E::B => fun_b(x),
    }
}

fn main() {
    use_enum(E::A, &[]);
}
