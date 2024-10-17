//! # Compiler Principle
//! This is a compiler for the SysY language, which is a subset of C language.
//! The compiler is implemented in Rust, and it can generate RISC-V assembly code.
//!
//! Repository: https://gitlab.eduxiji.net/pku2200010825/compiler2024.git

use backend::{emit_asm, AsmError};
use frontend::{build_ir, emit_ir, AstError};
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io;
use std::process::exit;

mod backend;
mod common;
mod frontend;

lalrpop_mod!(sysy);

#[derive(Debug)]
#[allow(dead_code)]
pub enum Error {
    InvalidArgs(String),
    InvalidFile(io::Error),
    Io(io::Error),
    Parse,
    Ir(frontend::AstError),
    Asm(backend::AsmError),
}

fn main() {
    let debug = true;
    if debug {
        _debug();
    } else {
        _main().unwrap();
    }
}

fn _debug() {
    if let Err(e) = _main() {
        match e {
            Error::Parse => exit(600),
            Error::Ir(err) => match err {
                AstError::AssignError(_) => exit(701),
                AstError::FunctionNotFoundError(_) => exit(702),
                AstError::IllegalConstExpError(_) => exit(703),
                AstError::UndefinedConstError(_) => exit(704),
                AstError::UndefinedVarError(_) => exit(705),
                AstError::UnknownError(_) => exit(706),
            },
            Error::Asm(err) => match err {
                AsmError::FunctionNotFound(_) => exit(801),
                AsmError::InvalidStackFrame => exit(802),
                AsmError::RegisterNotAssigned(_) => exit(803),
            },
            _ => exit(400),
        }
    }
}

fn _main() -> Result<(), Error> {
    let args = parse_cmd_args()?;
    let input = read_to_string(args.input).map_err(Error::InvalidFile)?;
    let output = if let Some(path) = args.output {
        let file = File::create(path).map_err(Error::InvalidFile)?;
        Box::new(file) as Box<dyn io::Write>
    } else {
        Box::new(io::stdout())
    };

    let ast = sysy::CompUnitParser::new().parse(&input).map_err(|_| Error::Parse)?;
    let mut program = build_ir(ast).map_err(Error::Ir)?;
    // println!("{:#?}", ast);
    match args.mode {
        Mode::Koopa => emit_ir(&mut program, output).map_err(Error::Io)?,
        Mode::RiscV => {
            let program = backend::build_asm(program).map_err(Error::Asm)?;
            emit_asm(program, output).map_err(Error::Io)?
        }
        Mode::Perf => todo!(),
    }
    Ok(())
}

fn parse_cmd_args() -> Result<CommandLineArgs, Error> {
    let mut cmd_args = CommandLineArgs::default();
    let mut args = args();
    args.next();
    match args.next().unwrap().as_str() {
        "-koopa" => cmd_args.mode = Mode::Koopa,
        "-riscv" => cmd_args.mode = Mode::RiscV,
        "-perf" => cmd_args.mode = Mode::Perf,
        _ => return Err(Error::InvalidArgs("Invalid mode".to_string())),
    }
    cmd_args.input = args.next().unwrap();
    args.next();
    cmd_args.output = args.next();
    Ok(cmd_args)
}

#[derive(Default)]
struct CommandLineArgs {
    mode: Mode,
    input: String,
    output: Option<String>,
}

#[derive(Debug, Default)]
enum Mode {
    #[default]
    Koopa,
    RiscV,
    Perf,
}
