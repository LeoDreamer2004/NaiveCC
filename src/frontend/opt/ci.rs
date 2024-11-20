use std::collections::HashMap;

use koopa::ir::builder::ValueBuilder;
use koopa::ir::{BasicBlock, Function, FunctionData, Value, ValueKind};
use koopa::opt::FunctionPass;

#[derive(Default)]
pub struct ConstantsInline {
    livemap: HashMap<Value, ValueKind>,            // Constant values
    worklist: Vec<(Value, BasicBlock, ValueKind)>, // Instructions to be processed
}

impl FunctionPass for ConstantsInline {
    fn run_on(&mut self, _: Function, func_data: &mut FunctionData) {
        for (&bb, node) in func_data.layout().bbs() {
            self.livemap.clear();
            for &inst in node.insts().keys() {
                let data = func_data.dfg().value(inst);
                match data.kind() {
                    // ValueKind::Binary(binary) => {
                    //     let lhs = func_data.dfg().value(binary.lhs());
                    //     let rhs = func_data.dfg().value(binary.rhs());
                    //     if let ValueKind::Integer(i1) = lhs.kind() {
                    //         if let ValueKind::Integer(i2) = rhs.kind() {
                    //             let i1 = i1.value();
                    //             let i2 = i2.value();
                    //             let r = match binary.op() {
                    //                 BinaryOp::NotEq => (i1 != i2) as i32,
                    //                 BinaryOp::Eq => (i1 == i2) as i32,
                    //                 BinaryOp::Gt => (i1 > i2) as i32,
                    //                 BinaryOp::Lt => (i1 < i2) as i32,
                    //                 BinaryOp::Ge => (i1 >= i2) as i32,
                    //                 BinaryOp::Le => (i1 <= i2) as i32,
                    //                 BinaryOp::Add => i1 + i2,
                    //                 BinaryOp::Sub => i1 - i2,
                    //                 BinaryOp::Mul => i1 * i2,
                    //                 BinaryOp::Div => i1 / i2,
                    //                 BinaryOp::Mod => i1 % i2,
                    //                 BinaryOp::And => i1 & i2,
                    //                 BinaryOp::Or => i1 | i2,
                    //                 BinaryOp::Xor => i1 ^ i2,
                    //                 // FIXME: Implement shift operations
                    //                 BinaryOp::Shl => todo!(),
                    //                 BinaryOp::Shr => todo!(),
                    //                 BinaryOp::Sar => i1 >> i2,
                    //             };
                    //         }
                    //     }
                    // }
                    ValueKind::Store(store) => {
                        let value = store.value();
                        let s_data = func_data.dfg().value(value);
                        match s_data.kind() {
                            ValueKind::Integer(_) => {
                                self.livemap.insert(store.dest(), s_data.kind().clone());
                            }
                            _ => {
                                self.livemap.remove(&store.dest());
                            }
                        }
                    }
                    ValueKind::Load(load) => {
                        if let Some(kind) = self.livemap.get(&load.src()) {
                            self.worklist.push((inst, bb, kind.clone()));
                        }
                    }
                    _ => {}
                }
            }
        }

        while let Some((inst, bb, kind)) = self.worklist.pop() {
            func_data.layout_mut().bb_mut(bb).insts_mut().remove(&inst);
            let builder = func_data.dfg_mut().replace_value_with(inst);
            match kind {
                ValueKind::Integer(int) => {
                    builder.integer(int.value());
                }
                _ => {}
            }
        }
    }
}
