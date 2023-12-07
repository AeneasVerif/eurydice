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

fn main() {
    let Foo { x, y } = mk_foo2();
    let expected = 0u32;
    mut_array(x);
    mut_foo(Foo { x, y });
    assert_eq!(x[0], expected);
}
