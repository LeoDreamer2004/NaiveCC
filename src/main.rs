use crate::frontend::{emit_ir, Error};
use frontend::build_program;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io;

mod backend;
mod frontend;

lalrpop_mod!(sysy);

fn main() -> Result<(), Error> {
    let args = parse_cmd_args()?;
    let input = read_to_string(args.input).map_err(|e| Error::InvalidFile(e))?;
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    // println!("{:#?}", ast);

    let mut program = build_program(ast)?;
    let output = if let Some(path) = args.output {
        let file = File::create(path).map_err(|e| Error::InvalidFile(e))?;
        Box::new(file) as Box<dyn io::Write>
    } else {
        Box::new(io::stdout())
    };
    emit_ir(&mut program, output)?;

    // assemble(program);
    Ok(())
}

fn parse_cmd_args() -> Result<CommandLineArgs, Error> {
    let mut cmd_args = CommandLineArgs::default();
    let mut args = args();
    args.next();
    match args.next().unwrap().as_str() {
        "-koopa" => cmd_args.mode = Mode::Koopa,
        "-riscv" => cmd_args.mode = Mode::RiscV,
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
    RiscV
}
