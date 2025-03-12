enum S {
    S1 { x1: u32, y1: u32 },
    S2 { x2: u32, y2: u32  }
}

fn mk1(x: u32, y: u32) -> S {
    S::S1 { x1: x, y1: y }
}

fn ident(x: S) -> S { x }

fn main() {
    match mk1(0, 0) {
        S::S1 { x1 : 0, y1 : 0 } => (),
        _ => panic!("oh noes")
    };
    match ident(S::S2 { x2: 0, y2: 0 }) {
        S::S2 { x2 : 0, y2 : 0 } => (),
        _ => panic!("oh noes")
    }
}
