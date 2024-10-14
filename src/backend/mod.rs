use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::ValueKind;
use koopa::ir::{FunctionData, Program, Value};
use riscv::*;
use util::AsmIdentify;
use std::io::Write;

mod riscv;
mod util;

macro_rules! write_output {
    ($output:expr, $($arg:tt)*) => {
        $output.write_fmt(format_args!($($arg)*)).unwrap();
    };
}

pub trait GenerateAsm {
    fn generate_on(&self, output: &mut impl Write);
}

impl GenerateAsm for Program {
    fn generate_on(&self, output: &mut impl Write) {
        write_output!(output, ".text\n");
        write_output!(output, ".globl\n");
        for &func in self.func_layout() {
            self.func(func).generate_on(output);
        }
    }
}

impl GenerateAsm for FunctionData {
    fn generate_on(&self, output: &mut impl Write) {
        write_output!(output, "{}:\n", self.original_ident());
        for (&_, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                generate_value_data(self.dfg(), inst, output);
            }
        }
    }
}

fn generate_value_data(dfg: &DataFlowGraph, value: Value, output: &mut impl Write) -> Register {
    match dfg.value(value).kind() {
        ValueKind::Integer(int) => {
            let int = int.value();
            write_output!(output, "li {}, {}\n", registers::A1, int);
            registers::A1
        }
        ValueKind::Return(ret) => {
            if let Some(value) = ret.value() {
                let mv = Mv {
                    rd: registers::A0,
                    rs: generate_value_data(dfg, value, output),
                };
                write_output!(output, "{}", mv.dump());
                write_output!(output, "{}", Ret {}.dump());
            }
            registers::A0
        }
        _ => unreachable!(),
    }
}

pub fn assemble(program: Program, mut output: impl Write) {
    program.generate_on(&mut output);
}
