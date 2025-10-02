struct S<U: ?Sized> {
    foo: u32,
    my_data: U,
}

type T = S<[u32]>;

fn main1() {
    let x = S { foo: 0, my_data: [0 ; 4] };
    let x: &T = &x;
    assert_eq!(x.my_data[3],0);
}

fn main2<const K:usize>() {
    let x = S { foo: 0, my_data: [0 ; K] };
    let x: &T = &x;
    assert_eq!(x.my_data[3],0);
}

fn main3() {
    let x: Box<[u32]> = Box::new([0; 4]);
    assert_eq!(x[3], 0);
}

fn main4<const K:usize>() {
    let x: Box<[u32]> = Box::new([0; K]);
    assert_eq!(x[3], 0);
}

fn main() {
    main1();
    main2::<5>();
    main3();
    main4::<5>();
}
