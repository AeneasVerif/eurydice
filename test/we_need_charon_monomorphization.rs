fn use_ref<T : ?Sized>(_t : &T) { }
fn supply_ref(s: &[i32]) { use_ref(s) }
fn main() { supply_ref(&[1,2,3]) }
