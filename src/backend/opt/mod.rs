mod algorithm;
mod cp;
mod dre;
mod helper;
mod immfix;
mod jump;
mod peephole;

use super::program::{AsmGlobal, AsmLocal};
pub use algorithm::AlgorithmOptimizer;
pub use cp::CopyPropagationOptimizer;
pub use dre::DeadRegisterOptimizer;
use helper::OptHelper;
pub use immfix::ImmFixOptimizer;
pub use jump::JumpOptimizer;
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

    pub fn run(&mut self, asm: &mut AsmGlobal) {
        for opt in self.optimizers.iter_mut() {
            match opt {
                Optimizer::Global(opt) => *asm = opt.run(asm),
                Optimizer::Local(opt) => {
                    for l in asm.locals_mut() {
                        *l = opt.run(l);
                    }
                }
            }
        }
    }
}
