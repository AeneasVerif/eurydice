pub struct MyStruct([u8; 5]);
fn use_it(x: &MyStruct) {
    let _ = x.0[0];
}

pub struct MyStruct2([u8; 5], u32);

fn use_it2(x: &MyStruct2) {
    let _ = x.0[0];
    let _ = x.1;
}

fn main() {
    let x = MyStruct([0; 5]);
    use_it(&x);
    let x = MyStruct2([0; 5], 2);
    use_it2(&x)
}
