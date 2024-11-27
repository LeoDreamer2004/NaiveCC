use crate::backend::instruction::{Inst, Label};
use crate::backend::program::AsmGlobal;

use super::super::dataflow::FunctionFlowGraph;
use super::GlobalOptimizer;

#[derive(Default)]
pub struct JumpOptimizer {
    work: (Label, Label),
}

impl GlobalOptimizer for JumpOptimizer {
    fn run(&mut self, asm: &AsmGlobal) -> AsmGlobal {
        let mut asm = asm.clone();
        let mut changed = true;
        while changed {
            let mut graph = FunctionFlowGraph::default();
            graph.build(&asm);
            self.mark(&graph, &mut asm);
            changed = self.simplify(&mut asm);
        }
        asm
    }
}

impl JumpOptimizer {
    fn mark(&mut self, graph: &FunctionFlowGraph, asm: &AsmGlobal) {
        for local in asm.locals() {
            if let Some(label) = local.label() {
                let froms = graph.from_duplicate(label);
                if froms.len() == 1 {
                    let from = froms[0].clone();
                    if graph.to_duplicate(&from).len() == 1 {
                        self.work = (from, label.clone());
                    }
                }
            }
        }
    }

    fn simplify(&mut self, asm: &mut AsmGlobal) -> bool {
        let (from, to) = self.work.clone();
        self.work = (Label::default(), Label::default());
        if from.is_empty() || to.is_empty() || from == to {
            return false;
        }

        let insts = asm.find_local(&to).unwrap().insts().clone();
        asm.remove_local(&to);

        let from = asm.find_local_mut(&from).unwrap();
        if matches!(from.insts().last(), Some(Inst::J(_))) {
            from.insts_mut().pop();
        }
        from.insts_mut().extend(insts);
        true
    }
}
