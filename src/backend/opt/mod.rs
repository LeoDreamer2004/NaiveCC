use super::instruction::AsmProgram;
mod immfix;
mod helper;
mod algorithm;

use helper::AsmHelper;
pub use immfix::ImmFixOptimizer;
pub use algorithm::AlgorithmOptimizer;

pub struct AsmOptimizeManager {
    optimizers: Vec<Box<dyn Optimizer>>,
}

pub trait Optimizer {
    fn run(&mut self, asm: AsmProgram) -> AsmProgram;
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
            asm = opt.run(asm);
        }
        asm
    }
}
