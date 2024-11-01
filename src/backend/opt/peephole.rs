use super::{AsmHelper, Optimizer};
use crate::backend::instruction::*;

pub struct PeepholeOptimizer {}

impl PeepholeOptimizer {
    pub fn new() -> Self {
        Self {}
    }
}

impl Optimizer for PeepholeOptimizer {
    fn run(&mut self, asm: AsmProgram) -> AsmProgram {
        let mut helper = AsmHelper::new(asm);
        let mut csr = helper.new_cursor();
        let mut go_next = true;
        while !csr.end() {
            if go_next {
                csr.next();
            } else {
                go_next = true;
            }
            let next = match csr.peek(1) {
                Some(next) => next,
                None => {
                    csr.next();
                    break;
                }
            };
            match (csr.current().clone(), next.clone()) {
                (Inst::Sw(Sw(r1, r2, imm)), Inst::Lw(Lw(r3, r4, imm2))) => {
                    if r2 == r4 && imm == imm2 {
                        csr.next();
                        csr.remove_cur();
                        go_next = false;
                        if r1 != r3 {
                            csr.insert(Inst::Mv(Mv(r3, r1)));
                        }
                    }
                }
                _ => {}
            }
        }
        helper.result()
    }
}
