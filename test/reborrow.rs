fn main() {
  let mut x = 0u8;
  let mut px = &mut x as *mut u8 as *const u8;
  unsafe {
    assert_eq!(0u8, *px);
    // Reborrow the pointer
    let py = &*px;
    assert_eq!(0u8, *py);
  }

  let val = 0u8;
  let b = Box::new(val);
  let pb = &*b;
  unsafe {
    assert_eq!(0u8, *pb);
    // Reborrow the pointer
    let pb2 = &*pb;
    assert_eq!(0u8, *pb2);
  }
}