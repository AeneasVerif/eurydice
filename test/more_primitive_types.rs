struct MorePrimitiveTypes {
    int128: i128,
    uint128: u128,
    c : char
}

fn match_u128(p : &MorePrimitiveTypes) -> i32 {
    match p.uint128 {
        0xffff | 0xffffffff => 3,
        0xffff_ffff_ffff_ffff_ffff_ffff_ffff => 4,
        0xffff_ffff_ffff_ffff_ffff_ffff_ffff_ffff => 5,
        _ => 1
    }
}

fn match_i128(p : &MorePrimitiveTypes) -> i32 {
    match p.int128 {
        -0xffff | -0xffffffff => 3,
        -0xffff_ffff_ffff_ffff => 4,
        -0xffff_ffff_ffff_ffff_ffff_ffff_fff => 5,
        _ => 1
    }
}

fn int128_operations(p : &mut MorePrimitiveTypes) -> i32 {
    p.int128 = p.int128 & 0xffff_ffff_1 - 0x1ff_ffff_ffff_ffff * 0x1 << 10;
    p.uint128 = p.uint128 & 0xffff_ffff_1 - 0x1f;
    let x = 1u128 >> p.uint128 << p.int128;
    x as i32
}

fn more_operations(p : &mut MorePrimitiveTypes) -> i32 {
    let mut result = 0;

    // Add
    result += (p.int128 + 1) as i32;
    result += (p.uint128 + 1) as i32;

    // Sub
    result += (p.int128 - 1) as i32;
    result += (p.uint128 - 1) as i32;

    // Mult
    result += (p.int128 * 2) as i32;
    result += (p.uint128 * 2) as i32;

    // Div
    result += (p.int128 / 2) as i32;
    result += (p.uint128 / 2) as i32;

    // Mod
    result += (p.int128 % 3) as i32;
    result += (p.uint128 % 3) as i32;

    // BShiftL
    result += (p.int128 << 1) as i32;
    result += (p.uint128 << 1) as i32;

    // BShiftR
    result += (p.int128 >> 1) as i32;
    result += (p.uint128 >> 1) as i32;

    // BAnd
    result += (p.int128 & 0xff) as i32;
    result += (p.uint128 & 0xff) as i32;

    // BOr
    result += (p.int128 | 0xff) as i32;
    result += (p.uint128 | 0xff) as i32;

    // BXor
    result += (p.int128 ^ 0xff) as i32;
    result += (p.uint128 ^ 0xff) as i32;

    // Eq
    if p.int128 == 0 {
        result += 1;
    }
    if p.uint128 == 0 {
        result += 1;
    }

    // Neq
    if p.int128 != 0 {
        result += 1;
    }
    if p.uint128 != 0 {
        result += 1;
    }

    // Lt
    if p.int128 < 0 {
        result += 1;
    }
    if p.uint128 < 1 {
        result += 1;
    }

    // Lte
    if p.int128 <= 0 {
        result += 1;
    }
    if p.uint128 <= 1 {
        result += 1;
    }

    // Gt
    if p.int128 > 0 {
        result += 1;
    }
    if p.uint128 > 1 {
        result += 1;
    }

    // Gte
    if p.int128 >= 0 {
        result += 1;
    }
    if p.uint128 >= 1 {
        result += 1;
    }

    // Neg
    result += (-p.int128) as i32;

    // BNot
    result += (!p.int128) as i32;
    result += (!p.uint128) as i32;

    result
}

fn use_more_primitive_types() {
    let s = b"ahello";
    let p = MorePrimitiveTypes {
        int128: -0xffff_ffff_ffff_ffff_ffff_ffff_fff,
        uint128: 0xffff_ffff_ffff_ffff_ffff_ffff_ffff,
        c: 'a'
    };
    match_u128(&p);
    match_i128(&p);
    assert!(p.c == s[0] as char);
}

fn main() {
    use_more_primitive_types();
}