enum Either {
  Left(i32),
  Right(bool),
}
enum Direction {
  Left(u64),
  Middle,
  Right(u64),
}
fn collision(a : Either, b : Direction) -> i32 {
  let x = match a {
    Either::Left(_) => 1,
    Either::Right(_) => 2,
  };
  let y = match b {
    Direction::Left(_) => 3,
    Direction::Middle => 0,
    Direction::Right(_) => 4,
  };
  x + y
}
fn main() {
  assert!(collision(Either::Left(10), Direction::Right(20)) > 0);
}
