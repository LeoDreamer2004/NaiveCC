//! # Compiler Principle
//! This is a compiler for the SysY language, which is a subset of C language.
//! The compiler is implemented in Rust, and it can generate RISC-V assembly code.
//!
//! Repository: https://gitlab.eduxiji.net/pku2200010825/compiler2024.git

use backend::emit_asm;
use frontend::{build_ir, emit_ir};
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io;

mod backend;
mod frontend;

lalrpop_mod!(sysy);

#[derive(Debug)]
#[allow(dead_code)]
pub enum Error {
    InvalidArgs(String),
    InvalidFile(io::Error),
    Io(io::Error),
    Parse(frontend::ParseError),
    Asm(backend::AsmError),
}

fn main() -> Result<(), Error> {
    let args = parse_cmd_args()?;
    let input = read_to_string(args.input).map_err(Error::InvalidFile)?;
    let output = if let Some(path) = args.output {
        let file = File::create(path).map_err(Error::InvalidFile)?;
        Box::new(file) as Box<dyn io::Write>
    } else {
        Box::new(io::stdout())
    };

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let mut program = build_ir(ast).map_err(Error::Parse)?;
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
