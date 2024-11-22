use super::{AsmHelper, Optimizer};
use crate::backend::program::AsmLocal;
use crate::backend::registers::FREE_REG;
use crate::backend::{instruction::*, is_imm12};

pub struct ImmFixOptimizer {}

impl ImmFixOptimizer {
    pub fn new() -> Self {
        Self {}
    }
}

impl Optimizer for ImmFixOptimizer {
    fn run(&mut self, asm: &AsmLocal) -> AsmLocal {
        let mut helper = AsmHelper::new(asm);
        let mut csr = helper.new_cursor();
        while !csr.end() {
            match csr.current().clone() {
                Inst::Addi(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::Add(rd, rs, FREE_REG));
                    }
                }
                Inst::Ori(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::Or(rd, rs, FREE_REG));
                    }
                }
                Inst::Andi(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::And(rd, rs, FREE_REG));
                    }
                }
                Inst::Xori(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::Xor(rd, rs, FREE_REG));
                    }
                }
                Inst::Lw(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::Add(FREE_REG, rs, FREE_REG));
                        csr.insert(Inst::Lw(rd, FREE_REG, 0));
                    }
                }
                Inst::Sw(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::Add(FREE_REG, rs, FREE_REG));
                        csr.insert(Inst::Sw(rd, FREE_REG, 0));
                    }
                }
                _ => {}
            }
            csr.next();
        }
        helper.result()
    }
}
