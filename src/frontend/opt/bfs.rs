use koopa::ir::{BasicBlock, Function, FunctionData, Value, ValueKind};
use koopa::opt::FunctionPass;
use std::collections::HashMap;

#[derive(Default)]
pub struct BlockGraphSimplifier {
    pub edges: HashMap<BasicBlock, Vec<BasicBlock>>,
}

impl FunctionPass for BlockGraphSimplifier {
    fn run_on(&mut self, _: Function, data: &mut FunctionData) {
        self.build_graph(data);
        while let Some((from, to)) = self.scan() {
            Self::merge(from, to, data);
        }
    }
}

impl BlockGraphSimplifier {
    fn build_graph(&mut self, data: &FunctionData) {
        for (&bb, node) in data.layout().bbs() {
            let mut edges = Vec::new();
            let last = node.insts().back_key().expect("Empty basic block");
            match data.dfg().value(*last).kind() {
                ValueKind::Jump(jump) => {
                    let target = jump.target();
                    edges.push(target);
                }
                ValueKind::Branch(branch) => {
                    let target = branch.true_bb();
                    edges.push(target);
                    let target = branch.false_bb();
                    edges.push(target);
                }
                _ => {}
            }
            self.edges.insert(bb, edges);
        }
    }

    fn scan(&mut self) -> Option<(BasicBlock, BasicBlock)> {
        for &bb in self.edges.keys() {
            if self.tos(bb).len() == 1 {
                let to = self.tos(bb)[0];
                if to == bb {
                    continue;
                }
                if self.froms(to).len() == 1 {
                    *self.edges.get_mut(&bb).unwrap() = self.edges.remove(&to).unwrap();
                    return Some((bb, to));
                }
            }
        }
        None
    }

    fn merge(from: BasicBlock, to: BasicBlock, data: &mut FunctionData) {
        let insts: Vec<Value> = all_insts!(data, to).iter().map(|(inst, _)| *inst).collect();
        if !data.dfg().bb(to).used_by().is_empty() {
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

    fn tos(&self, bb: BasicBlock) -> &Vec<BasicBlock> {
        self.edges.get(&bb).unwrap()
    }

    fn froms(&self, bb: BasicBlock) -> Vec<BasicBlock> {
        self.edges
            .iter()
            .filter_map(|(k, v)| v.contains(&bb).then_some(*k))
            .collect()
    }
}
