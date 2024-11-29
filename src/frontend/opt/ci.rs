use std::collections::HashMap;

use koopa::ir::builder::ValueBuilder;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Value, ValueKind};
use koopa::opt::FunctionPass;

#[derive(Default)]
pub struct ConstantsInline {
    livemap: HashMap<Value, i32>,            // Constant values
    worklist: Vec<(Value, BasicBlock, i32)>, // Instructions to be processed
}

impl FunctionPass for ConstantsInline {
    fn run_on(&mut self, _: Function, func_data: &mut FunctionData) {
        let mut changed = true;
        while changed {
            self.mark(func_data);
            changed = self.sweep(func_data);
        }
    }
}

impl ConstantsInline {
    fn mark(&mut self, func_data: &FunctionData) {
        // BUG: int x; y = x; x = 1 =>? y = 1
        for (&bb, node) in func_data.layout().bbs() {
            self.livemap.clear();
            for &inst in node.insts().keys() {
                let data = func_data.dfg().value(inst);
                match data.kind() {
                    ValueKind::Store(store) => {
                        let value = store.value();
                        if !func_data.dfg().values().contains_key(&value) {
                            continue;
                        }
                        let s_data = func_data.dfg().value(value);
                        match s_data.kind() {
                            ValueKind::Integer(int) => {
                                self.livemap.insert(store.dest(), int.value());
                            }
                            _ => {
                                self.livemap.remove(&store.dest());
                            }
                        }
                    }
                    ValueKind::Load(load) => {
                        if let Some(int) = self.livemap.get(&load.src()) {
                            self.worklist.push((inst, bb, *int));
                        }
                    }
                    ValueKind::Binary(binary) => {
                        let lhs = func_data.dfg().value(binary.lhs());
                        let rhs = func_data.dfg().value(binary.rhs());
                        if let (ValueKind::Integer(lhs), ValueKind::Integer(rhs)) =
                            (lhs.kind(), rhs.kind())
                        {
                            let lhs = lhs.value();
                            let rhs = rhs.value();
                            let int = match binary.op() {
                                BinaryOp::NotEq => (lhs != rhs) as i32,
                                BinaryOp::Eq => (lhs == rhs) as i32,
                                BinaryOp::Gt => (lhs > rhs) as i32,
                                BinaryOp::Lt => (lhs < rhs) as i32,
                                BinaryOp::Ge => (lhs >= rhs) as i32,
                                BinaryOp::Le => (lhs <= rhs) as i32,
                                BinaryOp::Add => lhs + rhs,
                                BinaryOp::Sub => lhs - rhs,
                                BinaryOp::Mul => lhs * rhs,
                                BinaryOp::Div => lhs / rhs,
                                BinaryOp::Mod => lhs % rhs,
                                BinaryOp::And => lhs & rhs,
                                BinaryOp::Or => lhs | rhs,
                                BinaryOp::Xor => lhs ^ rhs,
                                BinaryOp::Shl => lhs << rhs,
                                BinaryOp::Shr => (lhs as u32 >> rhs) as i32,
                                BinaryOp::Sar => lhs >> rhs,
                            };

                            self.worklist.push((inst, bb, int));
                        };
                    }
                    _ => {}
                }
            }
        }
    }

    fn sweep(&mut self, func_data: &mut FunctionData) -> bool {
        let mut res = false;
        while let Some((inst, bb, int)) = self.worklist.pop() {
            res = true;
            func_data.layout_mut().bb_mut(bb).insts_mut().remove(&inst);
            func_data.dfg_mut().replace_value_with(inst).integer(int);
        }
        res
    }
}
