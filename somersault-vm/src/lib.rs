use anyhow::{anyhow, bail, Result};
use std::{borrow::BorrowMut, collections::HashMap};

#[macro_use]
mod r#macro;
mod commands;

pub enum OpcodeResult {
    Continue,
    Pause,
    Stop,
}

type OpcodeHandler = fn(script: &mut Script, vm: &mut VM) -> Result<OpcodeResult>;

pub struct VM<'a> {
    opcode_map: HashMap<u16, OpcodeHandler>,
    print: &'a mut dyn FnMut(String),
}

pub struct Script<'a> {
    data: &'a [u8],
    pub scopes: Vec<Scope>,
}

pub struct Scope {
    ip: usize,
    cond_result: bool,
    pub lvars: [i32; VAR_COUNT],
}

pub const VAR_COUNT: usize = 32;
pub static mut SCRIPT_PARAMS: [i32; VAR_COUNT] = [0; VAR_COUNT];
pub static mut STACK: Vec<i32> = vec![];

impl<'a> Script<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self {
            data,
            scopes: vec![Scope::new(0)],
        }
    }

    pub fn get_ip(&self) -> usize {
        self.scopes.last().unwrap().ip
    }

    pub fn set_ip(&mut self, ip: i32) {
        self.scopes.last_mut().unwrap().ip = ip.abs() as _;
    }

    pub fn set_cond_result(&mut self, flag: bool) {
        self.scopes.last_mut().unwrap().cond_result = flag;
    }

    fn read_u8(&mut self) -> Result<u8> {
        read!(self, u8)
    }

    fn read_i8(&mut self) -> Result<i8> {
        read!(self, i8)
    }

    fn read_u16(&mut self) -> Result<u16> {
        read!(self, u16)
    }

    fn read_i16(&mut self) -> Result<i16> {
        read!(self, i16)
    }

    fn read_i32(&mut self) -> Result<i32> {
        read!(self, i32)
    }

    fn read_f32(&mut self) -> Result<f32> {
        read!(self, f32)
    }

    pub fn peek(&mut self) -> Result<u8> {
        self.data
            .get(self.get_ip())
            .cloned()
            .ok_or_else(|| anyhow!("Buffer overflow at index {}", self.get_ip()))
    }
}

pub fn skip_end_of_args(script: &mut Script) -> Result<()> {
    if script.peek()? == 0 {
        script.set_ip((script.get_ip() + 1) as i32);
        Ok(())
    } else {
        bail!("Expected end of arguments at index {}", script.get_ip());
    }
}

pub fn collect_params(script: &mut Script, count: usize) -> Result<()> {
    for i in 0..count {
        let dt = script.read_u8()?;

        match dt {
            1 => {
                unsafe { SCRIPT_PARAMS[i] = script.read_i32()? };
            }
            3 => match script.read_i16() {
                Ok(x) if x >= 0 && x < VAR_COUNT as _ => {
                    unsafe {
                        SCRIPT_PARAMS[i] = script.scopes.last_mut().unwrap().lvars[x as usize]
                    };
                }
                Ok(x) => bail!("Index {x} is out of bounds"),
                Err(e) => {
                    bail!(e)
                }
            },
            4 => {
                unsafe { SCRIPT_PARAMS[i] = script.read_i8()?.into() };
            }
            5 => {
                unsafe { SCRIPT_PARAMS[i] = script.read_i16()?.into() };
            }
            6 => {
                unsafe { SCRIPT_PARAMS[i] = std::mem::transmute(script.read_f32()?) };
            }
            21 => unsafe {
                let Some(value) = STACK.pop() else {
                    bail!("Stack is empty")
                };
                SCRIPT_PARAMS[i] = value;
            },
            _ => {
                unimplemented!()
            }
        }
    }

    Ok(())
}

pub fn store_params(script: &mut Script, count: usize) -> Result<()> {
    for i in 0..count {
        let dt = script.read_u8()?;

        match dt {
            3 => match script.read_i16() {
                Ok(x) if x >= 0 && x < VAR_COUNT as _ => {
                    unsafe {
                        script.scopes.last_mut().unwrap().lvars[x as usize] = SCRIPT_PARAMS[i]
                    };
                }
                Ok(x) => bail!("Index {x} is out of bounds"),
                Err(e) => {
                    bail!(e)
                }
            },
            20 => {
                unsafe { STACK.push(SCRIPT_PARAMS[i]) };
            }
            _ => {
                unimplemented!()
            }
        }
    }

    Ok(())
}

pub fn eat_variable(script: &mut Script, count: usize) -> Result<()> {
    for _ in 0..count {
        let dt = script.read_u8()?;

        match dt {
            3 => script.set_ip((script.get_ip() + 2) as i32),
            20 => {}
            _ => {
                unimplemented!()
            }
        }
    }

    Ok(())
}

pub fn get_pointer_to_variable(script: &mut Script) -> Result<*mut i32> {
    let dt = script.read_u8()?;

    match dt {
        3 => match script.read_i16() {
            Ok(x) if x >= 0 && x < VAR_COUNT as _ => {
                return Ok(script.scopes.last_mut().unwrap().lvars[x as usize].borrow_mut());
            }
            Ok(x) => bail!("Index {x} is out of bounds"),
            Err(e) => {
                bail!(e)
            }
        },
        20 => unsafe {
            STACK.push(0);
            return Ok(STACK.last_mut().unwrap().borrow_mut());
        },
        x => {
            bail!("Expected variable, got {x}")
        }
    }
}

impl Scope {
    pub fn new(ip: usize) -> Self {
        Self {
            ip,
            cond_result: false,
            lvars: Default::default(),
        }
    }
}

impl<'a> VM<'a> {
    pub fn new(printer: &'a mut dyn FnMut(String)) -> Self {
        use commands::*;
        let map: HashMap<u16, OpcodeHandler> = HashMap::from_iter(
            vec![
                (0x03E5, opcode_03e5 as _),
                (0x0006, opcode_0006 as _),
                (0x0007, opcode_0006 as _), // copy of 0006
                (0x004E, opcode_004e as _),
                (0x0093, opcode_0093 as _),
                (0x00AA, opcode_00aa as _),
                (0x00AB, opcode_00ab as _),
                (0x00BF, opcode_00bf as _),
                (0x0A8E, opcode_0a8e as _),
                (0x0A8F, opcode_0a8f as _),
                (0x0A90, opcode_0a90 as _),
                (0x0A91, opcode_0a91 as _),
                (0x0AB1, opcode_0ab1 as _),
                (0x2002, opcode_2002 as _),
                (0x2003, opcode_2003 as _),
                (0x2004, opcode_2004 as _),
                (0x0092, opcode_0092 as _)
            ]
            .into_iter(),
        );
        Self {
            opcode_map: map,
            print: printer,
        }
    }

    fn process(&mut self, script: &mut Script) -> Result<OpcodeResult> {
        if script.get_ip() == script.data.len() {
            return Ok(OpcodeResult::Stop);
        }
        let id = script.read_u16()?;
        if !self.opcode_map.contains_key(&id) {
            bail!("Unknown opcode {:04X}", id)
        }
        self.opcode_map[&id](script, self)
    }

    pub fn eval(&mut self, data: &[u8]) -> Result<()> {
        let mut script = Script::new(data);
        // println!("{:?}", data);
        loop {
            match self.process(&mut script) {
                Ok(OpcodeResult::Continue) => {}
                Ok(OpcodeResult::Pause) => {
                    break;
                }
                Ok(OpcodeResult::Stop) => {
                    break;
                }
                Err(e) => {
                    bail!(e)
                }
            }
        }
        Ok(())
    }
}
