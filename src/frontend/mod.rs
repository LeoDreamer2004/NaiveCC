pub mod ast;
mod builtin;
#[macro_use]
mod env;
mod dataflow;
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
    /// Error when the function is not found.
    FunctionNotFoundError(String),
    /// Error when the constant expression is illegal.
    IllegalConstExpError(String),
    /// Error when the variable is not found.
    SymbolNotFoundError(String),
    /// Error when the variable is wrongly initialized.
    InitializeError(String),
    /// Error when the variable is wrongly accessed.
    IllegalAccessError(String),
    /// Error when the loop stack is wrongly used.
    LoopStackError(String),
    /// Error when the variable is wrongly assigned.
    AssignError(String),
    /// Error when the type is error.
    TypeError(String),
    /// Unknown error.
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
    passman.register(Pass::Function(Box::new(BlockFlowSimplify::default())));
    passman.register(Pass::Function(Box::new(CopyPropagation::default())));
    passman.register(Pass::Function(Box::new(ConstantsFold::default())));
    passman.register(Pass::Function(Box::new(DeadCodeElimination::default())));
    passman.register(Pass::Function(Box::new(CommonSubexpression::default())));
    passman.register(Pass::Function(Box::new(UnusedCodeElimination::default())));
    passman.run_passes(&mut program);
    program
}

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), io::Error> {
    KoopaGenerator::new(output).generate_on(program)
}
