fn inner() -> Result<(), u8> {
    Err(1)
}

fn call_it() -> Result<(), u8> {
    inner()?;
    Ok(())
}

fn main() {
    assert_eq!(call_it(), Err(1))
}
