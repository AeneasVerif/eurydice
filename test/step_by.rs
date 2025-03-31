fn bar() {
    let mut i = 0;
    for j in (0..24).step_by(6) {
        i = i + j;
    }
    assert_eq!(i, 36);
}

fn main1() {
    bar();
    let mut i = 0;
    for j in (0..24).step_by(6) {
        i = i + j;
    }
    for j in (0..24).step_by(6) {
        i = i + j;
    }
}

fn main2() {
    main1();
    for j in (0..24).step_by(6) {
    }
    for j in (0..24).step_by(6) {
    }
}

fn main() {
    main2();
    for j in 0..24 {
    }
    for j in 0..24 {
    }
}
