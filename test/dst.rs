struct T2<U: ?Sized> {
    header: usize,
    my_data: U,
}

fn mk() -> Box<T2<[u32]>> {
    let mut x = T2 { header: 0, my_data: [0u32; 4] };
    x.my_data[1] = 2;
    let y: Box<T2<[u32]>> = Box::new(x);
    y
}

// ---

struct S<U: ?Sized> {
    foo: u32,
    my_data: U,
}

type T = S<[u32]>;

fn check_regular_field(x: Box<T>) {
    assert_eq!(x.foo, 0);
}

fn check_var_field(x: Box<T>) {
    assert_eq!(x.my_data[0], 0);
}

fn check_regular_field_ref(x: &T) {
    assert_eq!(x.foo, 0);
}

fn check_var_field_ref(x: &T) {
    assert_eq!(x.my_data[0], 0);
}

fn alloc() -> Box<T> {
    Box::new(S { foo: 0, my_data: [ 0; 4 ] })
}

// ---

fn main() {
    check_regular_field(alloc());
    check_var_field(alloc());

    let x = S { foo: 0, my_data: [ 0; 4 ] };
    let x: &T = &x;
    check_regular_field_ref(x);
    check_var_field_ref(x);

    assert_eq!((mk()).my_data[0], 0);
}
