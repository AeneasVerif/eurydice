/* TEST 1, with data types -- doesn't work yet */
struct Pair<T, U, const N: usize, const M: usize> {
    left: [ T; N ],
    right: [ U; M ]
}

fn mk_pairs<T: Copy, U: Copy, const N: usize, const M: usize> (x: T, y: U) -> Pair<T, T, N, N> {
    let a1 = [ x; N ];
    let a2 = [ y; M ];
    let p1 = Pair { left: a1, right: a2 };
    let p2 = Pair { left: a2, right: a1 };
    Pair { left: p1.left, right: p2.right }
}

fn main1() {
    let Pair { left, right } = mk_pairs::<u32,u64,2,4>(0u32, 0u64);
    let expected = 0u32;
    assert_eq!(left[0], expected);
    assert_eq!(left[1], expected);
    assert_eq!(right[0], expected);
    assert_eq!(right[1], expected);
}

/* TEST 2, too many trait bounds -- doesn't work yet */
/*
fn alloc2<const N: usize, const M: usize, T: Copy + Eq>(x: T) {
    let a1 = [x; N];
    let a2 = [x; M];
    if a1[0] != a2[0] {
        alloc2::<M, N, T>(x);
    }
}

fn main() {
    alloc2::<1,1, u32>(0);
}
*/

/* TEST 3 */
fn h<const FOO: usize>(x: u32) -> usize {
    FOO
}

fn i<const FOO: usize>(x: u32) -> u32 {
    x
}

fn f<const FOO: usize, const BAR: u32>(x: u32, y: usize) -> bool {
    let arr1 = [ x; FOO ];
    let arr2 = [ y; FOO ];
    arr1[0] == BAR && arr2[0] == FOO
}

fn g<const BAR: usize, const FOO: u32>(x: u32, y: usize) -> bool {
    f::<BAR, FOO>(x, y) && x == FOO && y == BAR
}

fn main3() {
    let x = f::<1, 2>(0, 0) && g::<3, 4>(0, 0);
}

/* TEST 4 (Franziskus) */
fn serialize<const OUT_LEN: usize>(re: &[u32]) -> [u8; OUT_LEN] {
  let mut out = [0u8; OUT_LEN];
  out[..4].copy_from_slice(&re[0].to_be_bytes());
  out[4..].copy_from_slice(&re[1].to_be_bytes());
  out
}

fn main() {
    let s: [u8; 8] = serialize(&[1, 2]);
    assert!(s[3] == 1);
    assert!(s[7] == 2);
}
