fn main() {
    let mut x = 0u8;
    let mut px = &mut x as *mut u8 as *const u8;
    unsafe {
        assert_eq!(0u8, *px);
    }
}
