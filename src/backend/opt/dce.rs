use super::super::dataflow::{GlobalFLowGraph, LiveVariableAnalyser, UseDefParser};
use super::super::program::{AsmGlobal, AsmLocal};
use super::super::registers::RegisterType;
use super::GlobalOptimizer;

#[derive(Default)]
pub struct DeadCodeOptimizer;

impl GlobalOptimizer for DeadCodeOptimizer {
    fn run(&mut self, asm: &AsmGlobal) -> AsmGlobal {
        let mut parser = UseDefParser::default();
        parser.parse(asm);
        let mut flow = GlobalFLowGraph::default();
        flow.build(asm);
        let mut analyser = LiveVariableAnalyser::new(flow);
        analyser.analyse(&parser);
        let mut res = AsmGlobal::new_from(asm);
        let ok_regs = RegisterType::Temp.all();

        for local in asm.locals() {
            if local.label().is_none() {
                res.new_local(local.clone());
                continue;
            }
            let mut res_l = AsmLocal::new_from(local);

            let label = local.label().as_ref().unwrap();
            let mut active_set = analyser.outs(label).clone();

            for inst in local.insts().iter().rev() {
                if let Some(rd) = inst.dest_reg() {
                    if ok_regs.contains(rd) && !active_set.remove(rd) {
                        // This instruction is dead
                        continue;
                    }
                }
                for rs in inst.src_regs() {
                    active_set.insert(*rs);
                }
                res_l.insts_mut().push(inst.clone());
            }
            res_l.insts_mut().reverse();
            res.new_local(res_l);
        }
        res
    }
}
