use koopa::ir::{BasicBlock, Function, FunctionData, ValueKind};
use koopa::opt::FunctionPass;
use std::collections::HashSet;

/// Performs dead code elimination.
#[derive(Default)]
pub struct DeadBlockElimination {
    worklist: Vec<BasicBlock>,
    liveset: HashSet<BasicBlock>,
}

impl FunctionPass for DeadBlockElimination {
    fn run_on(&mut self, _: Function, data: &mut FunctionData) {
        let mut changed = true;
        while changed {
            self.mark(data);
            changed = self.sweep(data);
        }
    }
}

impl DeadBlockElimination {
    fn mark(&mut self, data: &mut FunctionData) {
        let entry = match data.layout().entry_bb() {
            Some(entry) => entry,
            None => return,
        };
        self.liveset.insert(entry);
        self.worklist.push(entry);

        while let Some(bb) = self.worklist.pop() {
            // get the end inst
            let value = data.layout_mut().bb_mut(bb).insts_mut().back_key().copied();
            let last = data.dfg().value(value.unwrap());
            match last.kind() {
                ValueKind::Jump(jump) => {
                    let target = jump.target();
                    if !self.liveset.contains(&target) {
                        self.liveset.insert(target);
                        self.worklist.push(target);
                    }
                }
                ValueKind::Branch(branch) => {
                    let target = branch.true_bb();
                    if !self.liveset.contains(&target) {
                        self.liveset.insert(target);
                        self.worklist.push(target);
                    }
                    let target = branch.false_bb();
                    if !self.liveset.contains(&target) {
                        self.liveset.insert(target);
                        self.worklist.push(target);
                    }
                }
                _ => {}
            }
        }
    }

    fn sweep(&mut self, data: &mut FunctionData) -> bool {
        let mut bb_cur = data.layout_mut().bbs_mut().cursor_front_mut();
        let mut result = false;
        while let Some(bb) = bb_cur.key() {
            if !self.liveset.contains(bb) {
                result = true;
                bb_cur.remove_current();
            } else {
                bb_cur.move_next();
            }
        }
        result
    }
}
