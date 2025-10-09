trait Trait {
  fn method(&self);
}
fn use_trait(t: &dyn Trait) {
  t.method();
}
fn main() {

}