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
    let args = parse_cmd_args().unwrap();
    if args.local {
        _main(args).unwrap();
    } else {
        _handin(args);
    }
}

fn _handin(args: CommandLineArgs) {
    if let Err(e) = _main(args) {
        match e {
            Error::Parse => exit(60),
            Error::Ir(err) => match err {
                AstError::AssignError(_) => exit(71),
                AstError::FunctionNotFoundError(_) => exit(72),
                AstError::IllegalConstExpError(_) => exit(73),
                AstError::UndefinedConstError(_) => exit(74),
                AstError::UndefinedVarError(_) => exit(75),
                AstError::UnknownError(_) => exit(76),
                AstError::LoopStackError(_) => exit(77),
            },
            Error::Asm(err) => match err {
                AsmError::FunctionNotFound(_) => exit(81),
                AsmError::InvalidStackFrame => exit(82),
                AsmError::RegisterNotAssigned(_) => exit(83),
            },
            _ => exit(40),
        }
    }
}

fn _main(args: CommandLineArgs) -> Result<(), Error> {
    let input = read_to_string(args.input).map_err(Error::InvalidFile)?;
    let output = if let Some(path) = args.output {
        let file = File::create(path).map_err(Error::InvalidFile)?;
        Box::new(file) as Box<dyn io::Write>
    } else {
        Box::new(io::stdout())
    };

    let ast = sysy::CompUnitParser::new()
        .parse(&input)
        .map_err(|_| Error::Parse)?;
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
    cmd_args.output = match args.next() {
        Some(s) => {
            if s.as_str() == "-local" {
                cmd_args.local = true;
                None
            } else {
                s.into()
            }
        }
        _ => None,
    };
    Ok(cmd_args)
}

#[derive(Default)]
struct CommandLineArgs {
    mode: Mode,
    input: String,
    output: Option<String>,
    local: bool,
}

#[derive(Debug, Default)]
enum Mode {
    #[default]
    Koopa,
    RiscV,
    Perf,
}
