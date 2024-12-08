use super::super::dataflow::GlobalFLowGraph;
use super::super::instruction::{Inst, Label};
use super::super::program::AsmGlobal;
use super::GlobalOptimizer;

#[derive(Default)]
pub struct JumpOptimizer;

impl GlobalOptimizer for JumpOptimizer {
    fn run(&mut self, asm: &AsmGlobal) -> AsmGlobal {
        let mut graph = GlobalFLowGraph::default();
        let mut asm = asm.clone();
        graph.build(&asm);
        while let Some((from, to)) = self.mark(&graph, &asm) {
            self.simplify(&mut asm, from, to);
            graph.build(&asm);
        }
        asm
    }
}

impl JumpOptimizer {
    fn mark(&mut self, graph: &GlobalFLowGraph, asm: &AsmGlobal) -> Option<(Label, Label)> {
        for (label, _) in asm.labeled_locals() {
            let froms = graph.from_duplicate(&label);
            if froms.len() == 1 {
                let from = froms[0].clone();
                if from != label && graph.to_duplicate(&from).len() == 1 {
                    return Some((from, label));
                }
            }
        }
        None
    }

    fn simplify(&mut self, asm: &mut AsmGlobal, from: Label, to: Label) {
        let insts = asm.find_local(&to).unwrap().insts().clone();
        asm.remove_local(&to);

        let from = asm.find_local_mut(&from).unwrap();
        if matches!(from.insts().last(), Some(Inst::J(_))) {
            from.insts_mut().pop();
        }
        from.insts_mut().extend(insts);
    }
}
