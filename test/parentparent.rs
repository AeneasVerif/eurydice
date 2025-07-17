trait A: B {
}

trait B: C {
}

trait C {
    fn foo(&mut self);
}

fn f<U: A> (mut x: U) {
    x.foo()
}

fn main() {}
