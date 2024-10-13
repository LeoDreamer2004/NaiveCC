use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::{builder_traits::*, ValueKind};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};
use riscv::*;

use std::io::Write;

mod riscv;

macro_rules! write_output {
    ($output:expr, $($arg:tt)*) => {
        $output.write_fmt(format_args!($($arg)*)).unwrap();
    };
}

pub trait GenerateAsm {
    fn generate(&self, output: &mut impl Write);
}

impl GenerateAsm for Program {
    fn generate(&self, output: &mut impl Write) {
        for &func in self.func_layout() {
            self.func(func).generate(output);
        }
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, output: &mut impl Write) {
        for (&bb, node) in self.layout().bbs() {
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
            }
            registers::A0
        }
        _ => unreachable!(),
    }
}

pub fn assemble(program: Program) {
    program.generate(&mut std::io::stdout());
}
