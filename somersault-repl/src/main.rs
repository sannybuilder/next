use somersault_core::{backends, toolchain};
use somersault_vm::VM;
use std::{env, path::Path};

fn eval(input: String) {
    let mut backend = backends::SaCsBackend::default();
    match toolchain::run(input.as_bytes(), &mut backend) {
        Ok(_) => {
            let mut cb = |s: String| println!("{s}");
            let mut vm = VM::new(&mut cb);
            match vm.eval(&backend.buf) {
                Ok(_) => {}
                Err(e) => {
                    println!("EvalError: {}", e);
                }
            }
        }
        Err(e) => println!("CompileError: {e}"),
    }
}

fn main() {
    use std::io::{stdin, stdout, Write};

    let args: Vec<_> = env::args().collect();
    // read from input file
    if args.len() > 1 {
        match std::fs::read_to_string(Path::new(&args[1])) {
            Ok(code) => {
                eval(code);
            }
            Err(e) => println!("{}", e),
        };
        return;
    }

    // read from stdin
    loop {
        let mut s = String::new();
        print!("Enter expression: ");
        let _ = stdout().flush();
        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");
        if let Some('\n') = s.chars().next_back() {
            s.pop();
        }
        if let Some('\r') = s.chars().next_back() {
            s.pop();
        }

        if s.is_empty() {
            break;
        }

        eval(s);
    }
}
