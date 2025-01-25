use anyhow::{bail, Result};
use somersault_core::{backends, toolchain};
use std::{env, path::Path};

fn compile(input: String, definitions: String) -> Result<Vec<u8>> {
    let mut backend = backends::SaCsBackend::new(definitions);
    match toolchain::run(input.as_bytes(), &mut backend) {
        Ok(_) => Ok(backend.buf),
        Err(e) => bail!("{e}"),
    }
}

fn run(file_name: &str, definitions: String) -> Result<()> {
    let path = Path::new(file_name);
    let code = std::fs::read_to_string(path)?;
    let buf = compile(code, definitions)?;
    std::fs::write(path.with_extension("cs"), buf)?;
    Ok(())
}

fn watch(file_name: &str, definitions: String) -> Result<()> {
    use std::time::Duration;

    let path = Path::new(file_name);
    let mut last_modified = path.metadata()?.modified()?;

    loop {
        let metadata = path.metadata()?;
        if metadata.modified()? != last_modified {
            last_modified = metadata.modified()?;
            let time = std::time::SystemTime::now();
            match run(file_name, definitions.clone()) {
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
    let definitions = std::include_str!("../../data/sa.json");

    // Usage: ssc <input> [--watch]
    if args.len() > 1 {
        let file_name = &args[1];

        if let Err(e) = run(file_name, definitions.to_string()) {
            println!("{e}");
        }
        if args.contains(&"--watch".to_string()) {
            println!("Watching for changes... Press Ctrl+C to stop.");
            watch(file_name, definitions.to_string())?;
        }
    } else {
        println!("Usage: ssc <input>");
    }

    Ok(())
}
