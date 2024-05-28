#[derive(PartialEq, Debug)]
#[repr(u32)]
pub enum Enum { A = 0 }

fn main() {
    let expected = Enum::A;
    assert_eq!(expected, expected)
}
