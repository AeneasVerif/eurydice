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

struct T2<U: ?Sized> {
    header: usize,
    data: U,
}

fn mk() -> Box<T2<[u32]>> {
    let x = T2 { header: 0, data: [0u32; 4] };
    let y: Box<T2<[u32]>> = Box::new(x);
    y
}

fn main() {
    check(alloc());
    assert_eq!((mk()).data[0], 0);
}
