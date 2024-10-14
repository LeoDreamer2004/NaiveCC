use super::riscv::*;
use super::util::AsmIdentify;
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::ValueKind;
use koopa::ir::{FunctionData, Program, Value};
use std::io::Write;

pub trait GenerateAsm {
    fn generate_on(&self, output: &mut impl Write) -> Result<(), std::io::Error>;
}

impl GenerateAsm for Program {
    fn generate_on(&self, output: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(output, ".text")?;
        writeln!(output, ".globl main")?;
        for &func in self.func_layout() {
            self.func(func).generate_on(output)?;
        }
        Ok(())
    }
}

impl GenerateAsm for FunctionData {
    fn generate_on(&self, output: &mut impl Write) -> Result<(), std::io::Error> {
        writeln!(output, "{}:", self.original_ident())?;
        for (&_, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                generate_value_data(self.dfg(), inst, output)?;
            }
        }
        Ok(())
    }
}

fn generate_value_data(
    dfg: &DataFlowGraph,
    value: Value,
    output: &mut impl Write,
) -> Result<Register, std::io::Error> {
    match dfg.value(value).kind() {
        ValueKind::Integer(int) => {
            let int = int.value();
            writeln!(output, "li {}, {}", registers::A1, int)?;
            Ok(registers::A1)
        }
        ValueKind::Return(ret) => {
            if let Some(value) = ret.value() {
                let mv = Mv {
                    rd: registers::A0,
                    rs: generate_value_data(dfg, value, output)?,
                };
                writeln!(output, "{}", mv.dump())?;
                writeln!(output, "{}", Ret {}.dump())?;
            }
            Ok(registers::A0)
        }
        _ => unreachable!(),
    }
}

pub fn emit_asm(program: Program, mut output: impl Write) -> Result<(), std::io::Error> {
    program.generate_on(&mut output)
}
