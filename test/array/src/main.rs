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

// fn mk_incr2<const K: usize>() -> [ u32; K ] {
//     let j = 1;
//     core::array::from_fn(|i| i as u32 + j)
// }

fn mk_incr<const K: usize>() -> [ u32; K ] {
    core::array::from_fn(|i| i as u32)
}

fn main() {
    // XXX1
    let Foo { x, y } = mk_foo2();
    let expected = 0u32;
    mut_array(x);
    // XXX2
    mut_foo(Foo { x, y });
    assert_eq!(x[0], expected);
    let a: [ u32; 10 ] = mk_incr();
    let expected = 9;
    // XXX3
    assert_eq!(a[9], expected);
    // let a: [ u32; 10 ] = mk_incr2();
    // let expected = 10;
    // assert_eq!(a[9], expected);
}
