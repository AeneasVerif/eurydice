struct Foo<const K: usize>;

trait MyFnOnce {
    fn call_once(self) -> u32;
}

impl<const K: usize> MyFnOnce for Foo<K> {
    fn call_once(self) -> u32 {
        0
    }
}

fn from_fn<F>(f: F)
where
    F: MyFnOnce,
{
    f.call_once();
}

fn main() {
    from_fn(Foo::<10>);
}
