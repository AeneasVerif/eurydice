trait FnMut<F, T>: FnOnce<F, T> {
    fn call_mut(&mut self, arg: F) -> T;
}

trait FnOnce<F, T>: Foo<F, T> {
    fn call_once(&mut self, arg: F) -> T;
}

trait Foo<F, T> {
    fn foo(&mut self, arg: F) -> T;
}

fn f<F, T, U: FnMut<F, T>>(mut x: U, y: F) -> T {
    x.call_mut(y)
}

fn main() {
}
