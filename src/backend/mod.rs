mod assign;
mod constants;
mod dataflow;
mod env;
mod frames;
mod generate;
mod instruction;
mod location;
mod manager;
mod opt;
mod program;
mod registers;

use constants::*;
use env::Environment;
use generate::EntityAsmGenerator;
use koopa::ir::Program;
use opt::AsmOptimizeManager;
use program::AsmProgram;
use std::io;

#[derive(Debug)]
pub enum AsmError {
    /// Trying to get the address of a value that is not found.
    NullLocation(String),
    /// Stack frame is invalid
    InvalidStackFrame,
    /// Stack overflow
    StackOverflow,
}

/// Generate assembly code for the given IR program.
pub fn build_asm(program: Program) -> Result<AsmProgram, AsmError> {
    let mut asm = AsmProgram::new();
    let mut env = Environment::new(&program);
    program.generate_on(&mut env, &mut asm)?;
    Ok(asm)
}

/// Optimize the assembly code.
pub fn opt_asm(program: AsmProgram) -> AsmProgram {
    let mut optman = AsmOptimizeManager::default();
    optman.run(program)
}

/// Emit the assembly code to the given output.
pub fn emit_asm(program: AsmProgram, output: impl io::Write) -> Result<(), io::Error> {
    program.emit(output)
}
