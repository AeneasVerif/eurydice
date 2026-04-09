fn main() {
    // i8 wrapping operations
    assert_eq!(100i8.wrapping_add(27), 127);
    assert_eq!(100i8.wrapping_add(100), -56);
    assert_eq!((-128i8).wrapping_add(-1), 127);

    assert_eq!(10i8.wrapping_sub(20), -10);
    assert_eq!((-128i8).wrapping_sub(1), 127);
    assert_eq!(127i8.wrapping_sub(-1), -128);

    assert_eq!(10i8.wrapping_mul(5), 50);
    assert_eq!(100i8.wrapping_mul(2), -56);
    assert_eq!((-128i8).wrapping_mul(-1), -128);

    assert_eq!(5i8.wrapping_neg(), -5);
    assert_eq!((-128i8).wrapping_neg(), -128);

    // i16 wrapping operations
    assert_eq!(30000i16.wrapping_add(2768), -32768);
    assert_eq!((-32768i16).wrapping_add(-1), 32767);

    assert_eq!((-32768i16).wrapping_sub(1), 32767);
    assert_eq!(32767i16.wrapping_sub(-1), -32768);

    assert_eq!(200i16.wrapping_mul(200), -25536);
    assert_eq!((-32768i16).wrapping_mul(-1), -32768);

    assert_eq!(5i16.wrapping_neg(), -5);
    assert_eq!((-32768i16).wrapping_neg(), -32768);

    // i32 wrapping operations
    assert_eq!(2_000_000_000i32.wrapping_add(2_000_000_000), -294967296);
    assert_eq!((-2_147_483_648i32).wrapping_add(-1), 2_147_483_647);

    assert_eq!((-2_147_483_648i32).wrapping_sub(1), 2_147_483_647);
    assert_eq!(2_147_483_647i32.wrapping_sub(-1), -2_147_483_648);

    assert_eq!(100_000i32.wrapping_mul(100_000), 1_410_065_408);
    assert_eq!((-2_147_483_648i32).wrapping_mul(-1), -2_147_483_648);

    assert_eq!(5i32.wrapping_neg(), -5);
    assert_eq!((-2_147_483_648i32).wrapping_neg(), -2_147_483_648);

    // i64 wrapping operations
    assert_eq!(
        5_000_000_000_000_000_000i64.wrapping_add(5_000_000_000_000_000_000),
        -8_446_744_073_709_551_616
    );
    assert_eq!((-9_223_372_036_854_775_808i64).wrapping_add(-1), 9_223_372_036_854_775_807);

    assert_eq!((-9_223_372_036_854_775_808i64).wrapping_sub(1), 9_223_372_036_854_775_807);
    assert_eq!(9_223_372_036_854_775_807i64.wrapping_sub(-1), -9_223_372_036_854_775_808);

    assert_eq!(
        3_000_000_000i64.wrapping_mul(3_000_000_000),
        9_000_000_000_000_000_000
    );
    assert_eq!((-9_223_372_036_854_775_808i64).wrapping_mul(-1), -9_223_372_036_854_775_808);

    assert_eq!(5i64.wrapping_neg(), -5);
    assert_eq!((-9_223_372_036_854_775_808i64).wrapping_neg(), -9_223_372_036_854_775_808);
}
