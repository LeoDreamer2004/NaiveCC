use super::{AsmHelper, Optimizer};
use crate::backend::instruction::*;
use crate::backend::program::AsmLocal;

pub struct PeepholeOptimizer {}

impl PeepholeOptimizer {
    pub fn new() -> Self {
        Self {}
    }
}

impl Optimizer for PeepholeOptimizer {
    fn run(&mut self, asm: &AsmLocal) -> AsmLocal {
        let mut helper = AsmHelper::new(asm);
        let mut csr = helper.new_cursor();
        let mut go_next = false;
        while !csr.end() {
            if go_next {
                csr.next();
            } else {
                go_next = true;
            }
            let next = match csr.peek(1) {
                Some(next) => next,
                None => {
                    csr.go_to_end();
                    break;
                }
            };
            match (csr.current().clone(), next.clone()) {
                (Inst::Sw(r1, r2, imm), Inst::Lw(r3, r4, imm2)) => {
                    if r2 == r4 && imm == imm2 {
                        csr.next();
                        csr.remove_cur();
                        go_next = false;
                        if r1 != r3 {
                            csr.insert(Inst::Mv(r3, r1));
                        }
                    }
                }
                (Inst::Lw(r1, r2, imm), Inst::Sw(r3, r4, imm2)) => {
                    if r1 == r3 && r2 == r4 && imm == imm2 {
                        csr.next();
                        csr.remove_cur();
                        go_next = false;
                    }
                }
                (Inst::Mv(r1, r2), Inst::Mv(r3, r4)) => {
                    if r1 == r4 && r2 == r3 {
                        csr.next();
                        csr.remove_cur();
                        go_next = false;
                    }
                }
                (i1, i2) => {
                    if let Some(r1) = i1.dest_reg() {
                        if let Some(r2) = i2.dest_reg() {
                            if r1 == r2 && !i2.src_regs().contains(&r1) {
                                csr.remove_cur();
                            }
                        }
                    }
                }
            }
        }
        helper.result()
    }
}
