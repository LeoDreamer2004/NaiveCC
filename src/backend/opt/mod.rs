mod algorithm;
mod helper;
mod immfix;
mod peephole;

use super::program::{AsmGlobal, AsmLocal, AsmProgram};
pub use algorithm::AlgorithmOptimizer;
use helper::AsmHelper;
pub use immfix::ImmFixOptimizer;
pub use peephole::PeepholeOptimizer;

pub struct AsmOptimizeManager {
    optimizers: Vec<Box<dyn Optimizer>>,
}

pub trait Optimizer {
    fn run(&mut self, asm: &AsmLocal) -> AsmLocal;
}

impl AsmOptimizeManager {
    pub fn new() -> Self {
        Self {
            optimizers: Vec::new(),
        }
    }

    pub fn add(&mut self, opt: Box<dyn Optimizer>) {
        self.optimizers.push(opt);
    }

    pub fn run(&mut self, asm: AsmProgram) -> AsmProgram {
        let mut asm = asm;
        for opt in self.optimizers.iter_mut() {
            let mut res = AsmProgram::new();
            for g in asm.globals_mut() {
                res.new_global(AsmGlobal::new_from(g));
                for l in g.locals_mut() {
                    *l = opt.run(l);
                }
            }
        }
        asm
    }
}
