use super::Backend;
use crate::compiler::Instruction;
use crate::argument::OpcodeArgument;
use anyhow::{Ok, Result};

enum DataType {
    INT32 = 1,
    GVAR = 2,
    LVAR = 3,
    INT8 = 4,
    INT16 = 5,
    FLOAT = 6,
    GVARARRAY = 7,
    LVARARRAY = 8,
    // GVARSTRARR = 12,
    LVARSTR = 0x11,
    STR16 = 0x0E,
}

impl<'a> Into<Vec<u8>> for &OpcodeArgument {
    fn into(self) -> Vec<u8> {
        match self {
            OpcodeArgument::INT32(v) => std::iter::once(DataType::INT32 as u8)
                .chain(v.to_le_bytes())
                .collect(),
            OpcodeArgument::LVAR(index, ty) => {
                let data_type = match ty {
                    crate::parser::ArgType::String => DataType::LVARSTR,
                    _ => DataType::LVAR,
                };

                std::iter::once(data_type as u8)
                    .chain((*index as i16).to_le_bytes())
                    .collect()
            }
            OpcodeArgument::INT(v) => match *v {
                v if v >= -128 && v <= 127 => std::iter::once(DataType::INT8 as u8)
                    .chain((v as i8).to_le_bytes())
                    .collect(),
                v if v >= -32768 && v <= 32767 => std::iter::once(DataType::INT16 as u8)
                    .chain((v as i16).to_le_bytes())
                    .collect(),
                _ => std::iter::once(DataType::INT32 as u8)
                    .chain(v.to_le_bytes())
                    .collect(),
            },
            OpcodeArgument::FLOAT(v) => std::iter::once(DataType::FLOAT as u8)
                .chain(v.to_le_bytes())
                .collect(),
            OpcodeArgument::LABEL(_) => {
                unreachable!("labels should be converted to int");
            }
            OpcodeArgument::STR(vec) => std::iter::once(DataType::STR16 as u8)
                .chain(std::iter::once(vec.len() as u8))
                .chain(vec.clone())
                .collect(),
            OpcodeArgument::ARRAY(v) => {
                let (name, index, size) = *v.to_owned();

                let (data_type, start_index, ty) = match name {
                    OpcodeArgument::GVAR(index, ty) => (DataType::GVARARRAY, index as i16 * 4, ty),
                    OpcodeArgument::LVAR(index, ty) => (DataType::LVARARRAY, index as i16, ty),
                    _ => {
                        unreachable!("only variables can be name of the array")
                    }
                };
                let ty = match ty {
                    crate::parser::ArgType::Float => 1,  //f
                    crate::parser::ArgType::String => 3, //v
                    _ => 0,
                };
                let (index_type, index) = match index {
                    OpcodeArgument::GVAR(index, _) => (0x80, index as i16 * 4),
                    OpcodeArgument::LVAR(index, _) => (0, index as i16),
                    _ => {
                        unreachable!("only variables can be name of the array")
                    }
                };

                std::iter::once(data_type as u8)
                    .chain(start_index.to_le_bytes())
                    .chain(index.to_le_bytes())
                    .chain(std::iter::once(size as u8))
                    .chain(std::iter::once(ty + index_type))
                    .collect()
            }
            OpcodeArgument::GVAR(index, _ty) => std::iter::once(DataType::GVAR as u8)
                .chain((*index as i16 * 4).to_le_bytes())
                .collect(),
        }
    }
}
pub struct SaCsBackend {
    pub buf: Vec<u8>,
    pub definitions: String
}

impl  SaCsBackend {
    pub fn new(definitions: String) -> Self {
        Self {
            buf: Vec::new(),
            definitions,
        }
    }
}

impl Backend for SaCsBackend {
    fn get_definitions(&self) -> String {
        self.definitions.clone()
    }
    fn process(&mut self, insts: Vec<crate::compiler::Instruction>) -> Result<()> {
        for inst in insts {
            match inst {
                Instruction::OpcodeInst(op) => {
                    self.buf.extend_from_slice(&op.id.to_le_bytes());
                    let a = op
                        .args
                        .iter()
                        .map(|a| <&OpcodeArgument as Into<Vec<u8>>>::into(a))
                        .flatten()
                        .collect::<Vec<_>>();

                    self.buf.extend_from_slice(&a);
                    if op.is_variadic {
                        self.buf.push(0);
                    }
                }
                Instruction::RawBytes(bytes) => {
                    self.buf.extend_from_slice(&bytes);
                }
                _ => {}
            }
        }
        Ok(())
    }
}
