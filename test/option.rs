type Val = [u8; 16];
struct S {
    v: Option<Val>,
}
impl S {
    fn option_ref(&self) -> Option<&Val> {
        self.v.as_ref()
    }
}

fn main(){}
