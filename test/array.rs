struct Foo {
    x: [u32; 2],
    y: [u32; 2],
}

fn mut_array(mut x: [u32; 2]) {
    x[0] = 1u32;
}

fn mut_foo(mut f: Foo) {
    f.x[0] = 1u32;
    let mut copy: [u32; 2] = f.y;
    copy[0] = 0u32;
}

fn mk_foo() -> Foo {
    let x = [0u32, 0u32];
    let y = [1u32, 1u32];
    Foo { x, y }
}

fn mk_foo2() -> Foo {
    mk_foo()
}

fn mk_incr2<const K: usize>() -> [u32; K] {
    let j = 1;
    core::array::from_fn(|i| i as u32 + j)
}

fn mk_incr<const K: usize>() -> [u32; K] {
    core::array::from_fn(|i| i as u32)
}

fn nested_from_fn<const K: usize>() -> [[usize; K]; K] {
    core::array::from_fn(|j| core::array::from_fn(|i| i+j))
}

fn plus_one<const K: usize>(x: [u32; K]) -> [u16; K] {
    x.map(|x| (x + 1) as u16)
}

fn const_eq<const K:usize>(x: [u32; K], y: [u32; K]) -> bool {
   x == y
}

pub fn fun(x: &[[u8; 32]]) -> u8 {
    x[0][0..1][0]
}

fn main() {
    // XXX1
    let Foo { x, y } = mk_foo2();
    let unsigned = 0u32;
    mut_array(x);
    // XXX2
    mut_foo(Foo { x, y });
    assert_eq!(x[0], unsigned);

    let a: [u32; 10] = mk_incr();
    // XXX3
    assert_eq!(a[9], 9);

    let a: [u32; 10] = mk_incr2();
    let expected = 10;
    assert_eq!(a[9], expected);

    // XXX4
    let a = plus_one([0u32]);
    assert_eq!(a[0], 1u16);

    // XXX5
    let a = nested_from_fn::<4>();
    assert_eq!(a[3][3], 6);
}

