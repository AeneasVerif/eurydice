fn use_debug(_d: &dyn core::fmt::Debug) { }
fn main() { use_debug(&&1); }
