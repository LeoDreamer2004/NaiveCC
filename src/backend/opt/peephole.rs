use std::collections::HashMap;

use super::{AsmHelper, LocalOptimizer};
use crate::backend::instruction::*;
use crate::backend::program::AsmLocal;

#[derive(Default)]
pub struct PeepholeOptimizer;

impl LocalOptimizer for PeepholeOptimizer {
    fn run(&mut self, asm: &AsmLocal) -> AsmLocal {
        let mut helper = AsmHelper::new(asm);
        let mut csr = helper.new_cursor();
        while !csr.end() {
            let next = match csr.peek(1) {
                Some(next) => next,
                None => {
                    csr.go_to_end();
                    break;
                }
            };
            if csr.current() == &Inst::Nop {
                csr.remove_cur();
                csr.next();
                continue;
            }
            match (csr.current().clone(), next.clone()) {
                (Inst::Sw(r1, r2, imm), Inst::Lw(r3, r4, imm2)) => {
                    if r2 == r4 && imm == imm2 {
                        csr.next();
                        csr.remove_cur();
                        if r1 != r3 {
                            csr.insert(Inst::Mv(r3, r1));
                        }
                    }
                }
                (Inst::Lw(r1, r2, imm), Inst::Sw(r3, r4, imm2)) => {
                    if r1 == r3 && r2 == r4 && imm == imm2 {
                        csr.next();
                        csr.remove_cur();
                    }
                }
                (Inst::Mv(r1, r2), Inst::Mv(r3, r4)) => {
                    if r1 == r4 && r2 == r3 {
                        csr.next();
                        csr.remove_cur();
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
            csr.next();
        }

        // remove the extra immediate loading
        let asm = helper.result();
        let mut helper = AsmHelper::new(&asm);
        let mut map = HashMap::new();
        let mut csr = helper.new_cursor();
        while !csr.end() {
            let inst = csr.current().clone();
            match inst {
                Inst::Li(r, imm) => {
                    if let Some(i) = map.get(&r) {
                        if imm == *i {
                            csr.remove_cur();
                        }
                    }
                    map.insert(r, imm);
                }
                Inst::Call(_) => {
                    map.clear();
                }
                inst => {
                    if let Some(r) = inst.dest_reg() {
                        map.remove(r);
                    }
                }
            }
            csr.next();
        }
        helper.result()
    }
}
