struct S<U: ?Sized> {
    foo: u32,
    data: U,
}

type T = S<[u32]>;

fn check(x: Box<T>) {
    assert_eq!(x.foo, 0);
    assert_eq!(x.data[0], 0);
}

fn alloc() -> Box<T> {
    Box::new(S { foo: 0, data: [ 0; 4 ] })
}

fn main() {
    check(alloc())
}
