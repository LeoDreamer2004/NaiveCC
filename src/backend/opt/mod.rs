mod algorithm;
mod dce;
mod helper;
mod immfix;
mod peephole;

use super::program::{AsmGlobal, AsmLocal, AsmProgram};
pub use algorithm::AlgorithmOptimizer;
pub use dce::DeadCodeOptimizer;
use helper::AsmHelper;
pub use immfix::ImmFixOptimizer;
pub use peephole::PeepholeOptimizer;

pub struct AsmOptimizeManager {
    optimizers: Vec<Optimizer>,
}

pub enum Optimizer {
    Local(Box<dyn LocalOptimizer>),
    Global(Box<dyn GlobalOptimizer>),
}

pub trait LocalOptimizer {
    fn run(&mut self, asm: &AsmLocal) -> AsmLocal;
}

pub trait GlobalOptimizer {
    fn run(&mut self, asm: &AsmGlobal) -> AsmGlobal;
}

impl AsmOptimizeManager {
    pub fn new() -> Self {
        Self {
            optimizers: Vec::new(),
        }
    }

    pub fn add(&mut self, opt: Optimizer) {
        self.optimizers.push(opt);
    }

    pub fn run(&mut self, asm: AsmProgram) -> AsmProgram {
        let mut asm = asm;
        for opt in self.optimizers.iter_mut() {
            for g in asm.globals_mut() {
                match opt {
                    Optimizer::Global(opt) => *g = opt.run(g),
                    Optimizer::Local(opt) => {
                        for l in g.locals_mut() {
                            *l = opt.run(l);
                        }
                    }
                }
            }
        }
        asm
    }
}
