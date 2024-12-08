use super::super::program::AsmLocal;
use super::super::registers::FREE_REG;
use super::super::{instruction::*, is_imm12};
use super::{OptHelper, LocalOptimizer};

#[derive(Default)]
pub struct ImmFixOptimizer;

impl LocalOptimizer for ImmFixOptimizer {
    fn run(&mut self, asm: &AsmLocal) -> AsmLocal {
        let mut helper = OptHelper::new(asm);
        let mut csr = helper.new_cursor();
        while !csr.end() {
            match csr.current().clone() {
                Inst::Addi(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::Add(rd, rs, FREE_REG));
                    } else if imm == 0 {
                        csr.remove_cur();
                        csr.insert(Inst::Mv(rd, rs));
                    }
                }
                Inst::Ori(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::Or(rd, rs, FREE_REG));
                    } else if imm == 0 {
                        csr.remove_cur();
                        if rd != rs {
                            csr.insert(Inst::Mv(rd, rs));
                        }
                    }
                }
                Inst::Andi(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::And(rd, rs, FREE_REG));
                    } else if imm == 0 {
                        csr.remove_cur();
                        csr.insert(Inst::Li(rd, 0));
                    }
                }
                Inst::Xori(rd, rs, imm) => {
                    if !is_imm12(imm) {
                        csr.remove_cur();
                        csr.insert(Inst::Li(FREE_REG, imm));
                        csr.insert(Inst::Xor(rd, rs, FREE_REG));
                    } else if imm == 0 {
                        csr.remove_cur();
                        if rd != rs {
                            csr.insert(Inst::Mv(rd, rs));
                        }
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
                Inst::Mv(rd, rs) => {
                    if rd == rs {
                        csr.remove_cur();
                    }
                }
                _ => {}
            }
            csr.next();
        }
        helper.result()
    }
}
