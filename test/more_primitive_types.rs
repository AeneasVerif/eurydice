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