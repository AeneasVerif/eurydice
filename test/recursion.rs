fn is_odd(y: i32) -> bool {
   if y < 0 { return is_odd(-y); }
   match y {
       0 => false,
       1 => true,
       y => is_odd(y - 2)
   }
}

fn main() {
    assert!(is_odd(5));
}
