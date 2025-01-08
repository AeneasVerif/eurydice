enum Gamma2 {
    V95_232 = 95_232,
    V261_888 = 261_888,
}

enum E1 {
    C1 = 0xffffffff,
    C2 = -0xffffffff,
    C3 = 0x0fffffff
}

#[derive(PartialEq)]
enum E2 {
    C1 = 0xff,
    C2 = -1,
}

enum E3 {
    C1 = 0xff
}

enum E4 {
    C1 = 0x7f,
    C2 = -0x7e
}

fn main() {
    assert_eq!(E2::C1 as isize, -1);
}
