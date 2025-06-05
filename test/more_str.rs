// This file describes the usage of multiple UTF-8 strings in Rust.

fn use_str(s: &str) {
  // This function takes a string slice and does trivial operations on it.
  assert!(s.len() > 2);
}

fn main() {
  // various UTF-8 strings
  let eng = "Hello, world!";
  let chn = "你好，世界！";
  let jpn = "こんにちは世界！";
  let mix = "Hello, 你好，こんにちは世界！123Hi";
  use_str(eng);
  use_str(chn);
  use_str(jpn);
  use_str(mix);
}
