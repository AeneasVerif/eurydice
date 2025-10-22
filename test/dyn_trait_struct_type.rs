trait Trait {
  fn method(&self);
}
// impl Trait for i32 {
//   fn method(&self) {
//     assert!(*self > 0);
//   }
// }
fn use_trait(t: &dyn Trait) {
  t.method();
}
fn main() {
  // use_trait(&100);
}