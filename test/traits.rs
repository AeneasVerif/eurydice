enum Foo { Foo1, Foo2 }

trait ToInt {
    fn to_int(&self) -> u32;
}

impl ToInt for Foo {
    fn to_int(&self) -> u32 {
        match self {
          | Foo::Foo1 => 1,
          | Foo::Foo2 => 2
        }
    }
}

impl ToInt for &[Foo] {
    fn to_int(&self) -> u32 {
        self[0].to_int() * self[1].to_int()
    }
}

fn main() {
    let foos = [Foo::Foo1, Foo::Foo2];
    // TODO: [..] (full range)
    // TODO: assert_eq -- interesting pattern where some intermediary computations need to be
    // allocated
    // assert_eq!((&foos[0..2]).to_int(), 2u32);
    if (&foos[0..2]).to_int() != 2u32 {
        panic!();
    }
}
