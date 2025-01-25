use crate::code_gen;

use super::{backends, compiler, parser, tokens};
use anyhow::Result;

pub fn run(source: &[u8], backend: &mut impl backends::Backend) -> Result<()> {
    tokens::tokenize(source.into())
        .and_then(|tokens| parser::parse(tokens))
        .and_then(|program| compiler::compile(program, backend.get_definitions()))
        .and_then(|instructions| {
            code_gen::transform(instructions, code_gen::CodeGenContext::default())
        })
        .and_then(|instructions| backend.process(instructions))
}
