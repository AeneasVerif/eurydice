struct S<U: ?Sized> {
    foo: u32,
    data: U,
}

type T1 = S<[u32]>;
type TS1 = S<[u32; 4]>;

type T2 = S<T1>;
type TS2 = S<TS1>;

type T3 = S<T2>;
type TS3 = S<TS2>;

fn main() {
    let x1 = TS1 { foo: 0, data: [0; 4]};
    let y1 : &T1 = &x1;
    assert_eq!(y1.data[1],0);

    let x2 = TS2 { foo: 0, data: x1};
    let y2 : &T2 = &x2;
    assert_eq!(y2.data.data[2],0);

    let x3 = TS3 { foo: 0, data: x2};
    let y3 : &T3 = &x3;
    assert_eq!(y3.data.data.data[3],0);
}
