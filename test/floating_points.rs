fn main() {
  let f : f32 = 1.0;
  let arr = [f; 100];
  let d = 1.0f64;
  let arr2 = [d; 100];
  assert_eq!(arr[0], 1.0);
  assert_eq!(arr2[0], 1.0);
  assert_eq!(arr.len(), 100);
  assert_eq!(arr2.len(), 100);
}