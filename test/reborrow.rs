use std::str::FromStr;

fn reborrow_rule() {
  let mut s = String::from_str("abc").unwrap();
  let r = &mut s;
  {
    let rr = &mut *r;
    println!("{rr}");
  }
  println!("r: {r}");
  println!("s: {s}");
}

fn main() {
    reborrow_rule();
}
