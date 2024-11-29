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
use opt::*;
use program::{AsmGlobal, AsmProgram};
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

fn opt_glb(glb: &mut AsmGlobal) {
    let mut man = AsmOptimizeManager::new();
    man.add(Optimizer::Global(Box::new(JumpOptimizer::default())));
    man.add(Optimizer::Local(Box::new(AlgorithmOptimizer::default())));
    man.add(Optimizer::Local(Box::new(PeepholeOptimizer::default())));
    man.add(Optimizer::Local(Box::new(PeepholeOptimizer::default())));
    man.add(Optimizer::Global(Box::new(DeadCodeOptimizer::default())));
    man.add(Optimizer::Local(Box::new(PeepholeOptimizer::default())));
    man.add(Optimizer::Local(Box::new(AlgorithmOptimizer::default())));
    man.add(Optimizer::Local(Box::new(ImmFixOptimizer::default())));
    man.run(glb);
}

/// Optimize the assembly code.
pub fn opt_asm(program: AsmProgram) -> AsmProgram {
    let mut asm = program;
    for g in asm.globals_mut() {
        opt_glb(g);
    }
    asm
}

/// Emit the assembly code to the given output.
pub fn emit_asm(program: AsmProgram, output: impl io::Write) -> Result<(), io::Error> {
    program.emit(output)
}
