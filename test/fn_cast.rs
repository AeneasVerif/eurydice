fn id_ref<'a, T>(x: &'a T) -> &'a T {
    x
}

fn applies<'a, 'b, T, R>(f : fn(&'a T) -> &'b R, arg : &'a T) -> &'b R {
  f(arg)
}

fn main() {
  assert_eq!(*applies(id_ref, &1), 1)
}
