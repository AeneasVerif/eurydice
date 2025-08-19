
fn sum_lst<const N : usize>(lst : &[usize;N]) -> usize {
  let mut sum = 0;
  for i in 0 .. N { sum = sum + lst[i]; }
  sum + N
}

fn more_sum_lst(l : &[i32; 3]) -> i32 {
  let mut sum = 0;
  for i in 0..3 { sum = sum + l[i]; }
  sum
}

fn empty_ptr(f : fn() -> i32) -> i32 {
  f()
}

fn unit_empty_ptr(f : fn()) {
  f()
}

fn id<R>(r : R) -> R { r }

fn compose_cg_apply<X : Copy,Y,const N : usize,Z>(
  f : fn(&[X;N]) -> Y, 
  g : fn(Y) -> Z, 
  arg : &[X;N]) -> Z {
  g(f(arg))
}

fn use_compose_cg() {
  let x = compose_cg_apply(sum_lst, id, &[1,2,3,4,5]);
  let y = compose_cg_apply(more_sum_lst, id, &[10,11,12]);
  assert_eq!(x, 20);
  assert_eq!(y, 33);
}

fn main() {
    use_compose_cg();
}
