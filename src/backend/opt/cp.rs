use std::collections::HashMap;

use super::super::instruction::Inst;
use super::super::program::AsmLocal;
use super::LocalOptimizer;

#[derive(Default)]
pub struct CopyPropagationOptimizer;

impl LocalOptimizer for CopyPropagationOptimizer {
    fn run(&mut self, asm: &AsmLocal) -> AsmLocal {
        let mut changed = true;
        let mut asm = asm.clone();
        while changed {
            changed = false;

            // livemap: copy dest -> copy src
            let mut livemap = HashMap::new();
            let mut res = AsmLocal::new_from(&asm);

            for i in asm.insts() {
                let mut inst = i.clone();
                for reg in inst.src_regs_mut() {
                    if let Some(r) = livemap.get(reg) {
                        if reg != r {
                            *reg = *r; // propagate
                            changed = true;
                        }
                    }
                }

                if let Some(reg) = i.dest_reg() {
                    livemap.remove(reg);
                    // the src has be covered
                    livemap.retain(|_, &mut v| v != *reg);
                }

                if matches!(inst, Inst::Call(_)) {
                    livemap.clear();
                }

                if let Inst::Mv(dest, src) = inst {
                    livemap.insert(dest, src); // copy
                }
                res.insts_mut().push(inst);
            }
            asm = res;
        }
        asm
    }
}
