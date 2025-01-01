use crate::compiler;
use anyhow::Result;

mod sanny_text;
mod sa_cs;

pub trait Backend {
    fn process(&mut self, insts: Vec<compiler::Instruction>) -> Result<()>;
}

pub use sanny_text::SannyTextBackend;
pub use sa_cs::SaCsBackend;
