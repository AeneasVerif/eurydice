trait A: B {
}

trait B: C {
}

trait C {
    fn foo(&mut self);
}

fn f<U: A> (x: U) {
}

fn main() {}
