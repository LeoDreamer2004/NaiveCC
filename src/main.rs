use crate::frontend::{build_program, emit_ir, Error};
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::{self};

mod ast;
mod backend;
mod frontend;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<(), Error> {
    let args = parse_cmd_args()?;
    let input = read_to_string(args.input).map_err(|e| Error::InvalidFile(e))?;
    let ast= sysy::CompUnitParser::new().parse(&input).unwrap();
    
    // println!("{:#?}", ast);

    let mut program = build_program(ast).unwrap();
    let output = if let Some(path) = args.output {
        let file = File::create(path).map_err(|e| Error::InvalidFile(e))?;
        Box::new(file) as Box<dyn io::Write>
    } else {
        Box::new(io::stdout())
    };
    let _ = emit_ir(&mut program, output);
    // assemble(program);
    Ok(())
}

fn parse_cmd_args() -> Result<CommandLineArgs, Error> {
    let mut cmd_args = CommandLineArgs::default();
    let mut args = args();
    args.next();
    cmd_args.mode = args.next().unwrap();
    cmd_args.input = args.next().unwrap();
    args.next();
    cmd_args.output = args.next();
    Ok(cmd_args)
}

#[derive(Default)]
struct CommandLineArgs {
    mode: String,
    input: String,
    output: Option<String>,
}
