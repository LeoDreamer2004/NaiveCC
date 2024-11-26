use std::collections::HashMap;

use koopa::ir::{Function, FunctionData, Type, TypeKind, Value, ValueKind};
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
            if let ValueKind::Alloc(_) = value_data.kind() {
                if let TypeKind::Pointer(ty) = value_data.ty().kind() {
                    match ty.kind() {
                        TypeKind::Int32 => {
                            let mut can_remove = true;
                            let mut used = Vec::new();
                            for u in value_data.used_by() {
                                used.push(*u);
                                if let ValueKind::Load(_) = data.dfg().value(*u).kind() {
                                    can_remove = false;
                                    break;
                                }
                            }
                            if can_remove {
                                self.to_remove.extend(used);
                                self.to_remove.push(value);
                            }
                        }
                        _ => {}
                    }
                } else {
                    unreachable!();
                }
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
