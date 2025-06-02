use somersault_core::{backends, toolchain};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile_text(s: String, definitions: String) -> Result<String, String> {
    let mut backend = backends::SannyTextBackend::new(definitions);
    match toolchain::run(s.as_bytes(), &mut backend) {
        Ok(_) => Ok(backend.lines.join("\n")),
        Err(e) => Err(format!("Error: {e}")),
    }
}

#[wasm_bindgen]
pub fn compile_binary(s: String, definitions: String) -> Result<String, String> {
    let mut backend = backends::SaCsBackend::new(definitions);
    match toolchain::run(s.as_bytes(), &mut backend) {
        Ok(_) => {
            let mut result = String::new();
            for b in backend.buf {
                result.push_str(format!("{:02X} ", b).as_str());
            }
            Ok(result)
        }
        Err(e) => Err(format!("Error: {e}")),
    }
}

#[wasm_bindgen]
pub fn get_ssc_version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}
