type Key = [ u32; 8 ];
const ZERO: Key = [ 0, 1, 2, 3, 4, 5, 6, 7 ];

fn test() -> [[u8; 200]; 4] {
    let mut out0 = [0u8; 200];
    let mut out1 = [0u8; 200];
    let mut out2 = [0u8; 200];
    let mut out3 = [0u8; 200];
    [out0, out1, out2, out3]
}

fn main() {
    let keys = [ [ ZERO; 3 ]; 3 ];
    for i in 0..3 {
        for j in 0..3 {
            for k in 0..8 {
                let actual = keys[i][j][k];
                let expected = k as u32;
                assert_eq!(actual, expected);
            }
        }
    }
}
