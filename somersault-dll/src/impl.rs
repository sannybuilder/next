use anyhow::{bail, Result};
use somersault_core::{backends, toolchain};
use std::{path::Path};

fn compile(input: String) -> Result<Vec<u8>> {
    let mut backend = backends::SaCsBackend::default();
    match toolchain::run(input.as_bytes(), &mut backend) {
        Ok(_) => Ok(backend.buf),
        Err(e) => bail!("{e}"),
    }
}

pub fn run(file_name: &str) -> Result<Vec<u8>> {
    let path = Path::new(file_name);
    let code = std::fs::read_to_string(path)?;
    std::fs::write("output.c", &code)?;
    compile(code)
}

