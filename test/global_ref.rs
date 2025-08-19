const C_VAL : &&&() = &&&();
static S_VAL : &&&i32 = &&&0;

fn main() {
  assert_eq!(***C_VAL, ());
  assert!(***S_VAL == 0);
}
