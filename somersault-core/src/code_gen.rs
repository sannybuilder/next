use anyhow::{bail, Ok, Result};
use std::collections::HashMap;

use crate::compiler::{Instruction, OpcodeInst};
use crate::argument::OpcodeArgument;

pub struct CodeGenContext {
    is_external: bool,
}

impl Default for CodeGenContext {
    fn default() -> Self {
        Self { is_external: true }
    }
}

impl Instruction {
    pub fn get_size(&self) -> usize {
        match self {
            Instruction::OpcodeInst(opcode_inst) => {
                let mut size = 2; // opcode

                for arg in &opcode_inst.args {
                    size += 1; // data type;

                    match arg {
                        OpcodeArgument::INT(x) => match *x {
                            x if x >= -128 && x <= 127 => {
                                size += 1;
                            }
                            x if x >= -32768 && x <= 32767 => {
                                size += 2;
                            }
                            _ => {
                                size += 4;
                            }
                        },
                        OpcodeArgument::FLOAT(_) => {
                            size += 4;
                        }
                        OpcodeArgument::LABEL(_) | OpcodeArgument::INT32(_) => {
                            size += 4;
                        }
                        OpcodeArgument::LVAR(..) | OpcodeArgument::GVAR(..) => {
                            size += 2;
                        }
                        OpcodeArgument::STR(vec) => {
                            size += vec.len() + 1; // length
                        }
                        OpcodeArgument::ARRAY(_) => {
                            size += 6;
                        }
                    }
                }

                if opcode_inst.is_variadic {
                    size += 1; // null byte
                }
                size
            }
            Instruction::Label(_) | Instruction::ArrayDecl(_) | Instruction::VarDecl(_) => 0,
            Instruction::RawBytes(bytes) => bytes.len(),
        }
    }
}

fn resolve_labels(instructions: &Vec<Instruction>) -> HashMap<String, usize> {
    let mut map = HashMap::default();

    let mut offset = 0;
    for inst in instructions {
        match inst {
            Instruction::Label(name) => {
                map.insert(name.clone(), offset);
            }
            inst => {
                offset += inst.get_size();
            }
        }
    }
    map
}

macro_rules! visit_arg {
    ($inst:expr; $pat: pat => $block: block) => {
        $inst
            .into_iter()
            .map(|inst| {
                Ok(match inst {
                    Instruction::OpcodeInst(opcode_inst) => Instruction::OpcodeInst(OpcodeInst {
                        args: opcode_inst
                            .args
                            .iter()
                            .cloned()
                            .map(|arg| match arg {
                                $pat => $block,
                                _ => Ok(arg),
                            })
                            .collect::<Result<Vec<_>>>()?,
                        ..opcode_inst
                    }),
                    inst => inst,
                })
            })
            .collect::<Result<Vec<_>>>()
    };
}

pub fn transform(
    instructions: Vec<Instruction>,
    context: CodeGenContext,
) -> Result<Vec<Instruction>> {
    [fix_zero_jump, replace_labels, patch_exports]
        .iter()
        .try_fold(instructions, |acc, transformer| transformer(acc, &context))
}

fn fix_zero_jump(
    mut instructions: Vec<Instruction>,
    _context: &CodeGenContext,
) -> Result<Vec<Instruction>> {
    for inst in &instructions {
        match inst {
            Instruction::Label(_) => {
                instructions.insert(
                    0,
                    Instruction::OpcodeInst(OpcodeInst {
                        id: 0,
                        args: vec![],
                        is_variadic: false,
                    }),
                );
                break;
            }
            Instruction::ArrayDecl(_) | Instruction::VarDecl(_) => {}
            Instruction::OpcodeInst(_) | Instruction::RawBytes(_) => {
                break;
            }
        }
    }

    Ok(instructions)
}

fn replace_labels(
    instructions: Vec<Instruction>,
    context: &CodeGenContext,
) -> Result<Vec<Instruction>> {
    let map = resolve_labels(&instructions);

    visit_arg!(instructions;
        OpcodeArgument::LABEL(name) => {
            let Some(offset) = map.get(&name) else {
                bail!("Can't find label {name}");
            };

            let new_offset = if context.is_external {
                *offset as i32 * -1
            } else {
                *offset as i32
            };
            Ok(OpcodeArgument::INT32(new_offset))
        }
    )
}

fn patch_exports(
    mut instructions: Vec<Instruction>,
    context: &CodeGenContext,
) -> Result<Vec<Instruction>> {
    let map = resolve_labels(&instructions);

    for inst in instructions.iter_mut() {
        match inst {
            Instruction::RawBytes(bytes) => {
                let mut offset = 0;
                let magic = vec![0xFF, 0x7F, 0xFE, 0x00, 0x00];
                if bytes.len() < 5 {
                    continue;
                }
                let custom_header = bytes[offset..offset + 5].to_vec();
                offset += 5;

                if custom_header == magic {
                    // check is this is a EXPT block 0x45, 0x58, 0x50, 0x54
                    let block_type = bytes[offset..offset + 4].to_vec();
                    offset += 4;

                    if block_type == vec![0x45, 0x58, 0x50, 0x54] {
                        // read 4 bytes from offset
                        unsafe {
                            let block_size = bytes
                                .as_ptr()
                                .offset(offset as _)
                                .cast::<i32>()
                                .read_unaligned();
                            offset += 4;

                            while offset < block_size as usize {
                                // read null-terminated string
                                let mut name = String::new();

                                for i in offset..bytes.len() {
                                    offset += 1;
                                    if bytes[i] == 0 {
                                        break;
                                    }
                                    name.push(bytes[i] as char);
                                }

                                // find function by name
                                let Some(function_address) = map.get(&name) else {
                                    bail!("Can't find label {name}");
                                };

                                let new_offset = if context.is_external {
                                    *function_address as i32 * -1
                                } else {
                                    *function_address as i32
                                };

                                // write function address
                                bytes
                                    .as_mut_ptr()
                                    .offset(offset as _)
                                    .cast::<i32>()
                                    .write_unaligned(new_offset);

                                offset += 4;

                                // skip inputs
                                offset += 1 + bytes[offset] as usize;
                                // skip outputs
                                offset += 1 + bytes[offset] as usize;

                                offset += 1; // skip flags
                                offset += 4; // skip unused
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    Ok(instructions)
}

#[cfg(test)]
mod suite {
    use crate::{argument::OpcodeArgument, compiler::{Instruction, OpcodeInst}};

    #[test]
    fn test_call() {
        let inst = Instruction::OpcodeInst(OpcodeInst {
            id: 0x0AB1,
            is_variadic: true,
            args: vec![OpcodeArgument::INT32(0)],
        });
        let size = inst.get_size();
        assert_eq!(size, 8);
    }
}
