trait Fun {
    const VAL: u8;
}

struct S {}
impl Fun for S {
    const VAL: u8 = 5;
}

fn sth<F: Fun>() -> u8 {
    F::VAL
}

fn call() -> u8 {
    sth::<S>()
}

fn main() {
    assert_eq!(call(), 5);
}
