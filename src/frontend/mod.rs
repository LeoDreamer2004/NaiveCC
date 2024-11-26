pub mod ast;
mod builtin;
#[macro_use]
mod env;
mod eval;
mod generate;
mod loops;
mod opt;
mod symbol;
mod transform;

use ast::CompUnit;
use env::Environment;
use generate::GenerateIr;
use koopa::back::KoopaGenerator;
use koopa::ir::Program;
use koopa::opt::{Pass, PassManager};
use opt::*;
use std::io;

#[derive(Debug)]
pub enum AstError {
    FunctionNotFoundError(String),
    IllegalConstExpError(String),
    SymbolNotFoundError(String),
    InitializeError(String),
    IllegalAccessError(String),
    LoopStackError(String),
    AssignError(String),
    TypeError(String),
    UnknownError(String),
}

pub fn build_ir(ast: CompUnit) -> Result<Program, AstError> {
    let mut env = Environment::default();
    ast.generate_on(&mut env)?;
    Ok(env.ctx.program)
}

pub fn opt_ir(program: Program) -> Program {
    let mut program = program;
    let mut passman = PassManager::new();
    passman.register(Pass::Function(Box::new(DeadBlockElimination::default())));
    passman.register(Pass::Function(Box::new(BlockFlowSimplifier::default())));
    passman.register(Pass::Function(Box::new(ConstantsInline::default())));
    passman.register(Pass::Function(Box::new(DeadCodeElimination::default())));
    passman.register(Pass::Function(Box::new(CommonSubexpression::default())));
    passman.register(Pass::Function(Box::new(UnreadCodeElimination::default())));
    passman.run_passes(&mut program);
    program
}

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), io::Error> {
    KoopaGenerator::new(output).generate_on(program)
}
