#![feature(try_with_capacity)]
fn test(cb_ciphertext: usize) -> Box<[u8]> {
    let pb_comp_ciphers = Vec::try_with_capacity(2 * cb_ciphertext);
    let mut pb_comp_ciphers = match pb_comp_ciphers {
        Result::Ok(pb_comp_ciphers) => pb_comp_ciphers,
        Result::Err(_) => panic!("oh noes")
    };
    pb_comp_ciphers.resize(2 * cb_ciphertext, 0u8);
    let mut pb_comp_ciphers = pb_comp_ciphers.into_boxed_slice();
    pb_comp_ciphers
}

fn main() {
    let b = test(4);
    assert_eq!(b[0], 0);
}
