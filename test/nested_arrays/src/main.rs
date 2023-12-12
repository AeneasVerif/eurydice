type Key = [ u32; 8 ];
const ZERO: Key = [ 0, 1, 2, 3, 4, 5, 6, 7 ];

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
