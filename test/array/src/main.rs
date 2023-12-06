struct Foo {
    x: [u32; 1],
    y: [u32; 1],
}

fn mut_array(mut x: [u32; 1]) {
    x[0] = 1u32;
}

fn mut_foo(mut f: Foo) {
    f.x[0] = 1u32;
}

fn main() {
    let x = [0u32];
    let y = [1u32];
    let expected = 0u32;
    mut_array(x);
    mut_foo(Foo { x, y });
    assert_eq!(x[0], expected);
}
