fn bar() {
    let mut i = 0;
    for j in (0..24).step_by(6) {
        i = i + j;
    }
    assert_eq!(i, 36);
}

fn main() {
    bar();
    let mut i = 0;
    for j in (0..24).step_by(6) {
        i = i + j;
    }
}
