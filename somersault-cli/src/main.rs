use anyhow::{bail, Result};
use somersault_core::{backends, toolchain};
use std::{env, path::Path};

fn compile(input: String) -> Result<Vec<u8>> {
    let mut backend = backends::SaCsBackend::default();
    match toolchain::run(input.as_bytes(), &mut backend) {
        Ok(_) => Ok(backend.buf),
        Err(e) => bail!("{e}"),
    }
}

fn run(file_name: &str) -> Result<()> {
    let path = Path::new(file_name);
    let code = std::fs::read_to_string(path)?;
    let buf = compile(code)?;
    std::fs::write(path.with_extension("cs"), buf)?;
    Ok(())
}

fn watch(file_name: &str) -> Result<()> {
    use std::time::Duration;

    let path = Path::new(file_name);
    let mut last_modified = path.metadata()?.modified()?;

    loop {
        let metadata = path.metadata()?;
        if metadata.modified()? != last_modified {
            last_modified = metadata.modified()?;
            let time = std::time::SystemTime::now();
            match run(file_name) {
                Err(e) => {
                    println!("{e}");
                }
                _ => println!("Compiled Successfully in {}ms", time.elapsed()?.as_millis()),
            }
        }

        std::thread::sleep(Duration::from_secs(1));
    }
}
fn main() -> Result<()> {
    let args: Vec<_> = env::args().collect();

    // Usage: ssc <input> [--watch]
    if args.len() > 1 {
        let file_name = &args[1];

        if let Err(e) = run(file_name) {
            println!("{e}");
        }
        if args.contains(&"--watch".to_string()) {
            println!("Watching for changes... Press Ctrl+C to stop.");
            watch(file_name)?;
        }
    } else {
        println!("Usage: ssc <input>");
    }

    Ok(())
}
