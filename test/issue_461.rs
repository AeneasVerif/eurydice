// Extract core::num's constants so these exercise generated macros.
pub fn max8() -> i8 {
    i8::MAX
}

pub fn max16() -> i16 {
    i16::MAX
}

pub fn equal8(x: i8) -> bool {
    x == i8::MAX
}

pub fn equal16(x: i16) -> bool {
    x == i16::MAX
}

pub fn unsigned_equal8(x: u8) -> bool {
    x == u8::MAX
}

pub fn unsigned_equal16(x: u16) -> bool {
    x == u16::MAX
}

// Unsigned maxima must be narrowed before widening or arithmetic as well.
pub fn wide8() -> u32 {
    u8::MAX as u32
}

pub fn wide16() -> u32 {
    u16::MAX as u32
}

pub fn half8() -> u8 {
    u8::MAX / 2
}

pub fn half16() -> u16 {
    u16::MAX / 2
}

fn main() {
    assert_eq!(max8(), 127);
    assert_eq!(max16(), 32767);
    assert!(i8::MIN == -128);
    assert!(i16::MIN == -32768);
    assert!(equal8(127));
    assert!(!equal8(-1));
    assert!(equal16(32767));
    assert!(!equal16(-1));
    assert!(unsigned_equal8(255));
    assert!(!unsigned_equal8(0));
    assert!(unsigned_equal16(65535));
    assert!(!unsigned_equal16(0));
    assert_eq!(wide8(), 255);
    assert_eq!(wide16(), 65535);
    assert_eq!(half8(), 127);
    assert_eq!(half16(), 32767);
}
