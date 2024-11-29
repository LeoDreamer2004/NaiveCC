use koopa::ir::{builder::ValueBuilder, FunctionData, Value, ValueKind};

pub trait ValueReplace {
    fn replace_value(&mut self, inst: Value, value: Value);
}

impl ValueReplace for FunctionData {
    fn replace_value(&mut self, inst: Value, value: Value) {
        macro_rules! replace {
            ($value_mut:expr) => {
                if *$value_mut == inst {
                    *$value_mut = value;
                }
            };
        }

        let uses = self.dfg().value(inst).used_by().clone();

        for u in uses {
            // Silly code here... but it works!
            let u_data = self.dfg().value(u);
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
            if self.dfg().values().contains_key(&u) {
                self.dfg_mut().replace_value_with(u).raw(u_data);
            }
        }
    }
}
