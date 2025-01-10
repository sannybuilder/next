use super::Backend;
use crate::compiler::Instruction;
use crate::argument::OpcodeArgument;
use crate::parser::ArgType;
use anyhow::{Ok, Result};

impl std::fmt::Display for OpcodeArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpcodeArgument::INT(int) | OpcodeArgument::INT32(int) => write!(f, "{}", int),
            OpcodeArgument::FLOAT(fl) => {
                //ensure that the float is displayed with .0 if it is a whole number
                if fl.fract() == 0.0 {
                    write!(f, "{:.1}", fl)
                } else {
                    write!(f, "{}", fl)
                }
            }
            OpcodeArgument::LABEL(label) => write!(f, "@{}", label),
            OpcodeArgument::GVAR(index, _ty) => {
                write!(f, "&{}", index * 4)
            }
            OpcodeArgument::LVAR(index, ty) => match ty {
                crate::parser::ArgType::String => {
                    write!(f, "{}@v", index)
                }
                _ => {
                    write!(f, "{}@", index)
                }
            },
            OpcodeArgument::STR(s) => {
                write!(f, "\"")?;
                for c in s {
                    write!(f, "{}", *c as char)?;
                }
                write!(f, "\"")
            }
            OpcodeArgument::ARRAY(b) => match &b.0 {
                OpcodeArgument::GVAR(name, arg_type) => match &b.1 {
                    OpcodeArgument::LVAR(index, _) => {
                        write!(
                            f,
                            "&{}({}@,1{})",
                            name * 4,
                            index,
                            match arg_type {
                                ArgType::Float => "f",
                                ArgType::String => "v",
                                _ => "i",
                            }
                        )
                    }
                    _ => {
                        unreachable!("index can only be local")
                    }
                },
                OpcodeArgument::LVAR(name, arg_type) => match &b.1 {
                    OpcodeArgument::LVAR(index, _) => {
                        write!(
                            f,
                            "{}@({}@,1{})",
                            name,
                            index,
                            match arg_type {
                                ArgType::Float => "f",
                                ArgType::String => "v",
                                _ => "i",
                            }
                        )
                    }
                    _ => {
                        unreachable!("index can only be local")
                    }
                },
                _ => {
                    unreachable!("only variables can be name of the array")
                }
            },
        }
    }
}

#[derive(Default)]
pub struct SannyTextBackend {
    pub lines: Vec<String>,
    pub definitions: String,
}

impl SannyTextBackend {
    pub fn new(definitions: String) -> Self {
        Self {
            lines: vec![],
            definitions,
        }
    }
}

impl Backend for SannyTextBackend {
    fn process(&mut self, insts: Vec<crate::compiler::Instruction>) -> Result<()> {
        let library = somersault_sbl::parse_from(&self.definitions)?;

        let commands = library
            .extensions
            .iter()
            .flat_map(|ext| ext.commands.iter())
            .collect::<Vec<_>>();

        let mut global_offset = 0;
        for inst in insts {
            let size = inst.get_size();
            match inst {
                Instruction::OpcodeInst(op) => {
                    let name = match commands.iter().find(|c| c.id == op.id & 0x7FFF) {
                        Some(c) => c.name.as_str(),
                        None => "/* <name not found> */",
                    };
                    let args = match name {
                        // sanny builder 4 requires the arguments to be in a specific order
                        // var = operand1 [operator] operand2
                        "INT_ADD" | "INT_SUB" | "INT_MUL" | "INT_DIV" | "MOD" | "BIT_AND"
                        | "BIT_OR" | "BIT_XOR" | "BIT_SHR" | "BIT_SHL" => {
                            let a = op.args[0].clone();
                            let b = op.args[1].clone();
                            let c = op.args[2].clone();
                            vec![c, a, b]
                        }
                        _ => op.args,
                    };
                    self.lines.push(format!(
                        "/* {:04}|{:04X} */ {}{} {}",
                        global_offset,
                        // size,
                        op.id,
                        if op.id > 0x7FFF { "NOT " } else { "" },
                        name,
                        args.iter()
                            .map(|x| format!("{}", x))
                            .collect::<Vec<_>>()
                            .join(" ")
                    ))
                }
                // _ => {}
                Instruction::Label(_label) => {
                    self.lines.push("".to_string());
                    if !_label.starts_with("$internal") {
                        self.lines.push(format!("// function {}", _label));
                    }
                    // self.lines.push(format!("\n:{}", global_offset)),
                }
                Instruction::VarDecl(_v) => {
                    // self.lines.push(format!("{} {}", v.ty, v.name)),
                }
                Instruction::ArrayDecl(_v) => {
                    // self.lines.push(format!("{} {}[{}]", v.ty, v.name, v.count))
                }
                Instruction::RawBytes(bytes) => {
                    self.lines.push("hex".to_string());
                    if bytes.iter().fold(0, |acc, &x| acc + x) == 0 {
                        self.lines.push(format!("00({})", bytes.len()));
                    } else {
                        self.lines.push(
                            bytes
                                .iter()
                                .map(|x| format!("{:02X}", x))
                                .collect::<Vec<_>>()
                                .join(" "),
                        );
                    }
                    self.lines.push("end".to_string());
                }
            }
            global_offset += size;
        }

        Ok(())
    }
}
