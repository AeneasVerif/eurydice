enum Struct {
  A(u8),
  B(Box<i32>),
  C
}
enum ThreeWays {
  Middle(i32),
  Left(u8),
  Right(Box<f64>),
}
fn use_struct(s: ThreeWays) -> Struct {
  match s {
    ThreeWays::Middle(i) => Struct::B(Box::new(i)),
    ThreeWays::Left(st) => Struct::A(st),
    _ => Struct::C,
  }
}
fn main() { }
