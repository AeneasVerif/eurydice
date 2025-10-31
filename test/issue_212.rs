fn id_mut<'a, T>(x : &'a T) -> &'a T { x }

fn main() {
  let arr : [i32; 3] = [1,2,3];
  let r : &[i32; 3] = id_mut(&arr);
  assert!(r[0] > 0);
}
