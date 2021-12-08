use std::fs::File;
use std::io::Read;

pub fn read_file(path: &str) -> std::io::Result<String> {
    let mut buffer = String::new();
    let mut f = File::open(path)?;
    f.read_to_string(&mut buffer)?;
    Ok(buffer)
}
