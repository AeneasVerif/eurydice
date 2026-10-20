// A break inside a match must exit the loop, not just the generated C switch.
pub fn break_in_match() -> u32 {
    let mut i = 0u32;
    let mut iterations = 0u32;
    loop {
        // Make the regression fail instead of looping forever.
        iterations += 1;
        assert!(iterations <= 4);
        match i {
            3 => break,
            _ => i += 1,
        }
    }
    i
}

fn main() {
    assert_eq!(break_in_match(), 3);
}
