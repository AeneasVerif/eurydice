/*
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

fn main() {
    let Pair { left, right } = mk_pairs::<u32,u64,2,4>(0u32, 0u64);
    assert_eq!(left[0], 0u32);
    assert_eq!(left[1], 0u32);
    assert_eq!(right[0], 0u32);
    assert_eq!(right[1], 0u32);
}
*/

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
