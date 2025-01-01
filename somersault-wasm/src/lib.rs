use somersault_core::{backends, toolchain};
use somersault_vm::VM;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn compile_text_with_definitions(s: String, definitions: String) -> Result<String, String> {
    let mut backend = backends::SannyTextBackend::new(definitions);
    match toolchain::run(s.as_bytes(), &mut backend) {
        Ok(_) => Ok(backend.lines.join("\n")),
        Err(e) => Err(format!("Error: {e}")),
    }
}

#[wasm_bindgen]
pub fn compile_text(s: String) -> Result<String, String> {
    let mut backend = backends::SannyTextBackend::default();
    match toolchain::run(s.as_bytes(), &mut backend) {
        Ok(_) => Ok(backend.lines.join("\n")),
        Err(e) => Err(format!("Error: {e}")),
    }
}

#[wasm_bindgen]
pub fn compile_binary(s: String) -> Result<String, String> {
    let mut backend = backends::SaCsBackend::default();
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
pub fn eval_code(s: String) -> Result<String, String> {
    let mut backend = backends::SaCsBackend::default();
    match toolchain::run(s.as_bytes(), &mut backend) {
        Ok(_) => {
            let mut output = vec![];
            let mut cb = |s: String| {
                output.push(s);
            };
            let mut vm = VM::new(&mut cb);
            match vm.eval(&backend.buf) {
                Ok(_) => Ok(output.join("\n")),
                Err(e) => Err(format!("EvalError: {}", e)),
            }
        }
        Err(e) => Err(format!("CompileError: {e}")),
    }
}

#[wasm_bindgen]
pub fn get_ssc_version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}
