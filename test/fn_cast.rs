fn id_ref<'a, T>(x: &'a T) -> &'a T {
    x
}

fn applies<'a, 'b, T>(f : fn(&'a T) -> &'b T, arg : &'a T) -> &'b T {
  f(arg)
}

fn main() {
  assert_eq!(*applies(id_ref, &1), 1)
}
