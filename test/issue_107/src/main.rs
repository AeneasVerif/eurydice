struct MyStruct {}

trait Fun {
    fn f() -> u8;
}

impl Fun for MyStruct {
    fn f() -> u8 {
        5
    }
}

fn generic_fun<F: Fun>() -> u8 {
    F::f()
}

fn main() {
}
