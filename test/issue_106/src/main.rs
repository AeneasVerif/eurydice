struct MyStruct {
    v: u8,
}

fn generate() -> MyStruct {
    MyStruct { v: 5 }
}

fn use_it(x: &MyStruct) -> u8 {
    x.v
}

fn use_ref() -> u8 {
    use_it(&generate())
}

fn main() {
}
