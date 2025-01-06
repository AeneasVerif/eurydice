trait T {
    type C;
    fn new() -> Self::C;
}
struct S {}
impl T for S {
    type C = [u8; 4];

    fn new() -> Self::C {
        [0; 4]//Self::C::default()
    }
}

fn fun<Type: T>() {}

fn caller() {
    fun::<S>();
}

fn associated_type() -> [u8; 4] {
     S::new()
}

fn main() {
    let r = associated_type()[0];
    assert_eq!(r, 0);
}
