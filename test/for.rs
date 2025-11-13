pub fn other(input: &[u8]) -> u8 {
    for i in 0..5 {
        if i == 2 {
            return 6;
        }
    }
    input[0]
}

fn main(){}
