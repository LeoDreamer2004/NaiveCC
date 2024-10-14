use koopa::back::KoopaGenerator;
use koopa::ir::Program;
use std::io;

pub mod ast;
mod generate;
mod transform;
#[macro_use]
mod util;

#[derive(Debug)]
#[allow(dead_code)]
pub enum Error {
    InvalidArgs,
    InvalidFile(io::Error),
    Parse(String),
    Io(io::Error),
}

pub use generate::build_program;

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), Error> {
    KoopaGenerator::new(output)
        .generate_on(program)
        .map_err(Error::Io)
}
