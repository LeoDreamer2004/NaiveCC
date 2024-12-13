use koopa::ir::{Function, FunctionData, TypeKind, Value, ValueKind};
use koopa::opt::FunctionPass;

#[derive(Default)]
pub struct UnreadCodeElimination {
    to_remove: Vec<Value>,
}

impl FunctionPass for UnreadCodeElimination {
    fn run_on(&mut self, _: Function, data: &mut FunctionData) {
        let mut changed = true;
        while changed {
            self.mark(data);
            changed = self.sweep(data);
        }
    }
}

impl UnreadCodeElimination {
    fn mark(&mut self, data: &FunctionData) {
        for (&value, value_data) in data.dfg().values() {
            if !matches!(value_data.kind(), ValueKind::Alloc(_)) {
                continue;
            }

            if let TypeKind::Pointer(ty) = value_data.ty().kind() {
                if !ty.is_i32() {
                    continue;
                }
                let mut can_remove = true;
                let mut used = Vec::new();
                for u in value_data.used_by() {
                    used.push(*u);
                    if matches!(data.dfg().value(*u).kind(), ValueKind::Load(_)) {
                        can_remove = false;
                        break;
                    }
                }
                if can_remove {
                    self.to_remove.extend(used);
                    self.to_remove.push(value);
                }
            } else {
                unreachable!()
            }
        }
    }

    fn sweep(&mut self, data: &mut FunctionData) -> bool {
        let mut bb_cur = data.layout_mut().bbs_mut().cursor_front_mut();
        while let Some(bb) = bb_cur.node_mut() {
            let mut inst_cur = bb.insts_mut().cursor_front_mut();
            while let Some(inst) = inst_cur.key() {
                if self.to_remove.contains(inst) {
                    inst_cur.remove_current();
                } else {
                    inst_cur.move_next();
                }
            }
            bb_cur.move_next();
        }

        let res = !self.to_remove.is_empty();
        for &value in &self.to_remove {
            data.dfg_mut().remove_value(value);
        }
        self.to_remove.clear();
        res
    }
}
