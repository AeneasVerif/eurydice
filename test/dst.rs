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

type T3 = S<[[u32; 3]]>;

fn alloc3() -> Box<T3> {
    Box::new(S { foo: 0, my_data: [ [0; 3]; 4 ] })
}

fn check_var_field_ref3(x: &T3) {
    assert_eq!(x.my_data[0][0], 0);
}

fn main3() {
    let x = alloc3();
    check_var_field_ref3(&x);
}

// ---

fn main4() {
    let x: Box<[u32]> = Box::new([0; 4]);
    let y = &x;
    assert_eq!(x[3], 0);
}

// ---

fn main() {
    check_regular_field(alloc());
    check_var_field(alloc());

    let x = S { foo: 0, my_data: [ 0; 4 ] };
    let x: &T = &x;
    check_regular_field_ref(x);
    check_var_field_ref(x);

    main3();

    assert_eq!((mk()).my_data[0], 0);
    assert_eq!((mk()).my_data[1], 2);

    main4();
}
