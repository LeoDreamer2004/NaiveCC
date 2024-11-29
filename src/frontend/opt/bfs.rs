use koopa::ir::{BasicBlock, Function, FunctionData, Value};
use koopa::opt::FunctionPass;

use crate::frontend::dataflow::FunctionFlowGraph;

#[derive(Default)]
pub struct BlockFlowSimplify {
    graph: FunctionFlowGraph,
}

impl FunctionPass for BlockFlowSimplify {
    fn run_on(&mut self, _: Function, data: &mut FunctionData) {
        self.graph = FunctionFlowGraph::default();
        self.graph.build(data);
        while let Some((from, to)) = self.scan() {
            Self::merge(from, to, data);
            self.graph.build(data);
        }
    }
}

impl BlockFlowSimplify {
    fn scan(&mut self) -> Option<(BasicBlock, BasicBlock)> {
        for bb in self.graph.bbs() {
            if self.graph.to(bb).len() == 1 {
                let to = self.graph.to(bb)[0];
                if to == bb {
                    continue;
                }
                if self.graph.from(to).len() == 1 {
                    return Some((bb, to));
                }
            }
        }
        None
    }

    fn merge(from: BasicBlock, to: BasicBlock, data: &mut FunctionData) {
        let insts: Vec<Value> = all_insts!(data, to).iter().map(|(inst, _)| *inst).collect();
        if data.dfg().bb(to).used_by().len() > 1 {
            // FIXME: in case of multiple uses (unknown bug)
            return;
        }
        let (end, _) = all_insts!(data, from).pop_back().unwrap();
        data.dfg_mut().remove_value(end);
        for inst in insts {
            add_inst!(data, from, inst);
        }
        data.layout_mut().bbs_mut().remove(&to);
        data.dfg_mut().remove_bb(to);
    }
}
