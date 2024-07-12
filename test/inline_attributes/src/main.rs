
#[inline]
fn f () -> u32 {
   1
}

#[inline(never)]
fn g () -> u32 {
   2
}

#[inline(always)]
fn h () -> u32 {
   3
}


fn main() {
    let r = f() + g() + h();
}
