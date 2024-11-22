mod algorithm;
mod helper;
mod immfix;
mod peephole;
use helper::AsmHelper;

use algorithm::AlgorithmOptimizer;
use immfix::ImmFixOptimizer;
use peephole::PeepholeOptimizer;

use super::program::{AsmGlobal, AsmLocal, AsmProgram};

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

    pub fn default() -> Self {
        let mut manager = AsmOptimizeManager::new();
        manager.add(Box::new(PeepholeOptimizer::new()));
        manager.add(Box::new(ImmFixOptimizer::new()));
        manager.add(Box::new(AlgorithmOptimizer::new()));
        manager
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
