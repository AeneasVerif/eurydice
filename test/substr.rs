fn main() {
  let str = "abc";
  let take = &str[1..];
  assert!(take.len() == 2);
}
