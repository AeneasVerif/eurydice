fn f1() {
    let mut x = [[0u8; 4]; 4];
    let (y0, _y1) = x.split_at_mut(2);
    y0[0][0] = 1;
    let actual = x[0][0];
    let expected = 1;
    assert_eq!(actual, expected);
}

fn f4<const K: usize>() {
    let mut x = [[0u8; K]; K];
    let (y0, _y1) = x.split_at_mut(2);
    y0[0][0] = 1;
    let actual = x[0][0];
    let expected = 1;
    assert_eq!(actual, expected);
}

fn f2() {
    let mut x = [[0u8; 4]; 4];
    let (y0, _y1) = x.split_at_mut(2);
    // generates a copy
    let mut z = y0[0];
    z[0] = 1;
    let actual = x[0][0];
    let expected = 0;
    assert_eq!(actual, expected);
    assert_eq!(z[0],1);
}

use std::convert::TryInto;
fn f3() {
    let x1 : &[u8] = &[0u8;4];
    let y1 : [u8;4] = x1.try_into().unwrap();
    
    let x2 : &[u8] = &[0u8;4];
    let y2 : &[u8;4] = x2.try_into().unwrap();
    
    let x3 : &mut [u8] = &mut [0u8;4];
    let y3 : [u8;4] = x3.try_into().unwrap();
    
    assert_eq!(y1[0],y2[0]);
    assert_eq!(y1[0],y3[0]);
}

fn f5<const K:usize>() {
    let x1 : &[u8] = &[0u8;K];
    let y1 : [u8;K] = x1.try_into().unwrap();
    
    let x2 : &[u8] = [0u8;K].as_slice();
    let y2 : &[u8;K] = x2.try_into().unwrap();
    
    assert_eq!(y1[0],y2[0]);
}

fn main() {
    f1();
    f2();
    f3();
    f4::<4>();
    f5::<4>();
}
