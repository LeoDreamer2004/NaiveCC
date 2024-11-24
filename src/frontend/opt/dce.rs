use koopa::ir::{Function, FunctionData, Value, ValueKind};
use koopa::opt::FunctionPass;
use std::collections::HashSet;

/// Performs dead code elimination.
#[derive(Default)]
pub struct DeadCodeElimination {
    worklist: Vec<Value>,
    liveset: HashSet<Value>,
}

impl FunctionPass for DeadCodeElimination {
    fn run_on(&mut self, _: Function, data: &mut FunctionData) {
        let mut changed = true;
        while changed {
            self.mark(data);
            changed = self.sweep(data);
        }
    }
}

impl DeadCodeElimination {
    fn mark(&mut self, data: &FunctionData) {
        for (v, value) in data.dfg().values() {
            if Self::is_critical_inst(value.kind()) {
                self.liveset.insert(*v);
                self.worklist.push(*v);
            }
        }

        while let Some(inst) = self.worklist.pop() {
            for u in data.dfg().value(inst).kind().value_uses() {
                if !data.dfg().values().contains_key(&u) {
                    // this is a global value
                    continue;
                }
                if !self.liveset.contains(&u) && data.dfg().value(u).kind().is_local_inst() {
                    self.liveset.insert(u);
                    self.worklist.push(u);
                }
            }
        }
    }

    fn sweep(&mut self, data: &mut FunctionData) -> bool {
        let mut removed = Vec::new();
        let mut bb_cur = data.layout_mut().bbs_mut().cursor_front_mut();
        while let Some(bb) = bb_cur.node_mut() {
            let mut inst_cur = bb.insts_mut().cursor_front_mut();
            while let Some(inst) = inst_cur.key() {
                if !self.liveset.contains(inst) {
                    removed.push(*inst);
                    inst_cur.remove_current();
                } else {
                    inst_cur.move_next();
                }
            }
            bb_cur.move_next();
        }
        let res = !removed.is_empty();
        for v in removed {
            // FIXME
            // data.dfg_mut().remove_value(v);
        }
        res
    }

    fn is_critical_inst(kind: &ValueKind) -> bool {
        matches!(
            kind,
            ValueKind::Store(_)
                | ValueKind::Call(_)
                | ValueKind::Branch(_)
                | ValueKind::Jump(_)
                | ValueKind::Return(_)
        )
    }
}
