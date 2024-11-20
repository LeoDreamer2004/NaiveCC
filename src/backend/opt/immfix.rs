use super::{AsmHelper, Optimizer};
use crate::backend::registers::FREE_REG;
use crate::backend::{instruction::*, is_imm12};


pub struct ImmFixOptimizer {}

impl ImmFixOptimizer {
    pub fn new() -> Self {
        Self {}
    }
}

impl Optimizer for ImmFixOptimizer {
    fn run(&mut self, asm: AsmProgram) -> AsmProgram {
        let mut helper = AsmHelper::new(asm);
        let mut csr = helper.new_cursor();
        while !csr.end() {
            match csr.current().clone() {
                Inst::Addi(Addi(rd, rs, imm)) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(Li(FREE_REG, imm)));
                        csr.insert(Inst::Add(Add(rd, rs, FREE_REG)));
                    }
                },
                Inst::Ori(Ori(rd, rs, imm)) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(Li(FREE_REG, imm)));
                        csr.insert(Inst::Or(Or(rd, rs, FREE_REG)));
                    }
                },
                Inst::Andi(Andi(rd, rs, imm)) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(Li(FREE_REG, imm)));
                        csr.insert(Inst::And(And(rd, rs, FREE_REG)));
                    }
                },
                Inst::Xori(Xori(rd, rs, imm)) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(Li(FREE_REG, imm)));
                        csr.insert(Inst::Xor(Xor(rd, rs, FREE_REG)));
                    }
                },
                Inst::Lw(Lw(rd, rs, imm)) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(Li(FREE_REG, imm)));
                        csr.insert(Inst::Add(Add(FREE_REG, rs, FREE_REG)));
                        csr.insert(Inst::Lw(Lw(rd, FREE_REG, 0)));
                    }
                },
                Inst::Sw(Sw(rd, rs, imm)) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(Li(FREE_REG, imm)));
                        csr.insert(Inst::Add(Add(FREE_REG, rs, FREE_REG)));
                        csr.insert(Inst::Sw(Sw(rd, FREE_REG, 0)));
                    }
                },
                _ => {}
            }
            csr.next();
        }
        helper.result()
    }
}
