use koopa::ir::builder::ValueBuilder;
use koopa::ir::{BasicBlock, Function, FunctionData, TypeKind, Value, ValueKind};
use koopa::opt::FunctionPass;
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct CopyBroadcast {
    glb_broadcast: HashSet<Value>,  // Values that can be broadcasted
    livemap: HashMap<Value, Value>, // Copy dest -> copy src
    worklist: Vec<(Value, BasicBlock, Value)>, // Instructions to be processed
}

impl FunctionPass for CopyBroadcast {
    fn run_on(&mut self, _: Function, func_data: &mut FunctionData) {
        let mut changed = true;
        while changed {
            self.parse(func_data);
            self.mark(func_data);
            changed = self.sweep(func_data);
        }
    }
}

impl CopyBroadcast {
    fn parse(&mut self, func_data: &FunctionData) {
        let mut wrote_in = HashMap::new();
        for (&bb, node) in func_data.layout().bbs() {
            for &inst in node.insts().keys() {
                let data = func_data.dfg().value(inst);
                if let ValueKind::Store(store) = data.kind() {
                    wrote_in.entry(store.dest()).or_insert(Vec::new()).push(bb);
                }
            }
        }

        // If the value is only written in one block, then it can be globally broadcasted
        self.glb_broadcast.clear();
        for (value, bbs) in wrote_in {
            if bbs.len() == 1 {
                self.glb_broadcast.insert(value);
            }
        }
    }

    fn can_copy(value: Value, func_data: &FunctionData) -> bool {
        if func_data.dfg().values().contains_key(&value) {
            let data = func_data.dfg().value(value);
            return !matches!(data.ty().kind(), TypeKind::Pointer(_));
        }
        false
    }

    fn mark(&mut self, func_data: &FunctionData) {
        let mut reserved_value = HashSet::new();
        for (&bb, node) in func_data.layout().bbs() {
            for v in self.livemap.clone().keys() {
                if !self.glb_broadcast.contains(v) {
                    self.livemap.remove(v);
                }
            }
            for &inst in node.insts().keys() {
                let data = func_data.dfg().value(inst);
                match data.kind() {
                    ValueKind::Store(store) => {
                        let value = store.value();
                        // let s_data = func_data.dfg().value(value);
                        // if !s_data.ty().is_i32() {
                        //     continue;
                        // }
                        // if !Self::can_copy(store.value(), func_data) {
                            // continue;
                        // }
                        self.livemap.insert(store.dest(), value);
                    }
                    ValueKind::Load(load) => {
                        if let Some(value) = self.livemap.get(&load.src()) {
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
            macro_rules! replace {
                ($value_mut:expr) => {
                    if *$value_mut == inst {
                        *$value_mut = value;
                    }
                };
            }

            let uses = func_data.dfg().value(inst).used_by().clone();
            res = true;

            for u in uses {
                // Silly code here... but it works!
                let u_data = func_data.dfg().value(u);
                let mut u_data = u_data.clone();

                match u_data.kind_mut() {
                    ValueKind::Aggregate(aggregate) => {
                        for elem in aggregate.elems_mut() {
                            replace!(elem);
                        }
                    }
                    ValueKind::Load(load) => replace!(load.src_mut()),
                    ValueKind::Store(store) => {
                        replace!(store.value_mut());
                        replace!(store.dest_mut());
                    }
                    ValueKind::GetPtr(ptr) => {
                        replace!(ptr.index_mut());
                        replace!(ptr.src_mut());
                    }
                    ValueKind::GetElemPtr(elem_ptr) => {
                        replace!(elem_ptr.index_mut());
                        replace!(elem_ptr.src_mut());
                    }
                    ValueKind::Binary(binary) => {
                        replace!(binary.lhs_mut());
                        replace!(binary.rhs_mut());
                    }
                    ValueKind::Branch(branch) => replace!(branch.cond_mut()),
                    ValueKind::Call(call) => {
                        for arg in call.args_mut() {
                            replace!(arg);
                        }
                    }
                    ValueKind::Return(ret) => {
                        if let Some(value) = ret.value_mut() {
                            replace!(value);
                        }
                    }
                    _ => unreachable!(),
                };

                if func_data.dfg().values().contains_key(&u) {
                    func_data.dfg_mut().replace_value_with(u).raw(u_data);
                }
            }
            func_data.layout_mut().bb_mut(bb).insts_mut().remove(&inst);
            func_data.dfg_mut().remove_value(inst);
        }
        res
    }
}
