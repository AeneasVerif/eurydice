use std::path::PathBuf;

fn func(path: PathBuf) -> PathBuf {
  path.components().skip(1).collect()
}
fn main() { }
