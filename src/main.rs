use backend::emit_asm;
use frontend::{build_program, emit_ir};
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
    InvalidArgs,
    InvalidFile(io::Error),
    Parse(frontend::ParseError),
    Io(io::Error),
}

fn main() -> Result<(), Error> {
    let args = parse_cmd_args()?;
    let input = read_to_string(args.input).map_err(Error::InvalidFile)?;
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    // println!("{:#?}", ast);

    let mut program = build_program(ast).map_err(Error::Parse)?;
    let output = if let Some(path) = args.output {
        let file = File::create(path).map_err(Error::InvalidFile)?;
        Box::new(file) as Box<dyn io::Write>
    } else {
        Box::new(io::stdout())
    };

    match args.mode {
        Mode::Koopa => emit_ir(&mut program, output).map_err(Error::Io)?,
        Mode::RiscV => emit_asm(program, output).map_err(Error::Io)?,
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
        _ => return Err(Error::InvalidArgs),
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
