fn f() -> u32 {
    return 0;
}

fn main() {
    match f() {
        0 => (),
        1 => panic!(),
        _ => panic!(),
    }
}
