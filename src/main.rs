//! # Naive Compiler
//! This is a naive compiler for the SysY language, which is a subset of C language.
//! The compiler is implemented in Rust, and it can generate RISC-V assembly code.
//! Designed for Compiler Principles course (2024 autumn) in Peking University.
//!
//! Handin Repository: https://gitlab.eduxiji.net/pku2200010825/compiler2024
//! Open Source Repository: https://github.com/LeoDreamer2004/Naive-Compiler
//! License: MIT

use backend::{build_asm, emit_asm, opt_asm};
use frontend::{build_ir, emit_ir, opt_ir};
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io;

mod backend;
mod frontend;
mod utils;

lalrpop_mod!(sysy);

#[derive(Debug)]
#[allow(dead_code)]
pub enum Error {
    InvalidMode(String),
    InvalidFile(io::Error),
    Io(io::Error),
    Parse,
    Ir(frontend::AstError),
    Asm(backend::AsmError),
}

fn main() -> Result<(), Error> {
    let args = parse_args().unwrap();
    let input = read_to_string(args.input).map_err(Error::InvalidFile)?;

    // parse
    let ast = sysy::CompUnitParser::new()
        .parse(&input)
        .map_err(|_| Error::Parse)?;

    // frontend
    let mut ir = build_ir(ast).map_err(Error::Ir)?;
    ir = opt_ir(ir);

    match args.mode {
        Mode::Koopa => emit_ir(&mut ir, args.output).map_err(Error::Io)?,
        _ => {
            // backend
            let mut asm = build_asm(ir).map_err(Error::Asm)?;
            asm = opt_asm(asm);
            emit_asm(asm, args.output).map_err(Error::Io)?
        }
    }
    Ok(())
}

fn parse_args() -> Result<RuntimeArgs, Error> {
    let mut args = args();
    args.next();
    let mode = match args.next().unwrap().as_str() {
        "-koopa" => Mode::Koopa,
        "-riscv" => Mode::RiscV,
        "-perf" => Mode::Perf,
        u => return Err(Error::InvalidMode(u.into())),
    };
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();
    let output: Box<dyn io::Write> = if output == "-debug" {
        Box::new(io::stdout())
    } else {
        Box::new(File::create(output).map_err(Error::InvalidFile)?)
    };
    Ok(RuntimeArgs {
        mode,
        input,
        output,
    })
}

struct RuntimeArgs {
    mode: Mode,
    input: String,
    output: Box<dyn io::Write>,
}

enum Mode {
    Koopa,
    RiscV,
    Perf,
}
