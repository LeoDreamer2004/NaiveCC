use super::super::dataflow::{FunctionFlowGraph, GenKillParser, ReachDefiniteAnalyser};
use super::util::ValueReplace;
use koopa::ir::{BasicBlock, Function, FunctionData, Value, ValueKind};
use koopa::opt::FunctionPass;
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct CopyBroadcast {
    graph: FunctionFlowGraph,
    worklist: Vec<(Value, BasicBlock, Value)>, // Instructions to be processed
}

impl FunctionPass for CopyBroadcast {
    fn run_on(&mut self, _: Function, func_data: &mut FunctionData) {
        self.graph.build(func_data);
        let mut changed = true;
        while changed {
            // self.parse(func_data);
            self.mark(func_data);
            changed = self.sweep(func_data);
        }
    }
}

impl CopyBroadcast {
    fn mark(&mut self, func_data: &FunctionData) {
        let mut reserved_value = HashSet::new();
        let mut parser = GenKillParser::default();
        parser.parse(func_data);
        let mut flow = ReachDefiniteAnalyser::default();
        flow.build(&self.graph, &parser);

        for (&bb, node) in func_data.layout().bbs() {
            // defmap: copy dest -> copy srcs
            let mut defmap = HashMap::new();

            let ins = flow.ins(bb);
            for i in ins {
                match func_data.dfg().value(*i).kind() {
                    ValueKind::Store(store) => {
                        defmap
                            .entry(store.dest())
                            .or_insert(HashSet::new())
                            .insert(store.value());
                    }
                    _ => unreachable!(),
                }
            }

            // livemap: copy dest -> copy src (if only one)
            let mut livemap = HashMap::new();
            for (dest, srcs) in defmap {
                if srcs.len() == 1 {
                    livemap.insert(dest, *srcs.iter().next().unwrap());
                }
            }

            for &inst in node.insts().keys() {
                let data = func_data.dfg().value(inst);
                match data.kind() {
                    ValueKind::Store(store) => {
                        let value = store.value();
                        if func_data.dfg().values().contains_key(&store.dest()) {
                            livemap.insert(store.dest(), value);
                        }
                    }
                    ValueKind::Load(load) => {
                        if let Some(value) = livemap.get(&load.src()) {
                            if func_data.dfg().value(inst).used_by().contains(&value) {
                                // Used circularly, so we can't broadcast it
                                reserved_value.insert(*value);
                            } else {
                                self.worklist.push((inst, bb, *value));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // The value shouldn't be broadcasted if it's in reserved_value
        self.worklist
            .retain(|(_, _, value)| !reserved_value.contains(value));
    }

    fn sweep(&mut self, func_data: &mut FunctionData) -> bool {
        let mut res = false;
        while let Some((inst, bb, value)) = self.worklist.pop() {
            res = true;
            func_data.replace_value(inst, value);
            func_data.layout_mut().bb_mut(bb).insts_mut().remove(&inst);
            func_data.dfg_mut().remove_value(inst);
        }
        res
    }
}
