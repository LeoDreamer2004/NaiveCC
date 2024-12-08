use super::super::dataflow::{AvailableExpressions, EGenKillParser, FunctionFlowGraph};
use super::util::ValueReplace;
use koopa::ir::{BasicBlock, Function, FunctionData, Value, ValueKind};
use koopa::opt::FunctionPass;

#[derive(Default)]
pub struct CommonSubexpression {
    graph: FunctionFlowGraph,
}

impl FunctionPass for CommonSubexpression {
    fn run_on(&mut self, _: Function, func_data: &mut FunctionData) {
        if func_data.layout().entry_bb().is_none() {
            // the function is a declaration
            return;
        }
        self.graph.build(func_data);
        while let Some((bb, from, to)) = self.scan(func_data) {
            Self::eliminate(func_data, bb, from, to);
        }
    }
}

impl CommonSubexpression {
    fn scan(&self, func_data: &FunctionData) -> Option<(BasicBlock, Value, Value)> {
        let mut parser = EGenKillParser::default();
        parser.parse(func_data);
        let mut analyser = AvailableExpressions::default();
        analyser.build(&self.graph, &parser);

        for (&bb, node) in func_data.layout().bbs() {
            let mut availables = analyser.ins(bb).clone();
            for &inst in node.insts().keys() {
                let data = func_data.dfg().value(inst);
                for a in availables.clone() {
                    if a == inst {
                        continue;
                    }
                    if Self::is_equal(func_data.dfg().value(a).kind(), data.kind()) {
                        return Some((bb, inst, a));
                    }
                }
                EGenKillParser::update(inst, &mut availables, func_data);
            }
            assert_eq!(&availables, analyser.outs(bb));
        }
        None
    }

    fn eliminate(func_data: &mut FunctionData, bb: BasicBlock, from: Value, to: Value) {
        func_data.replace_value(from, to);
        func_data.layout_mut().bb_mut(bb).insts_mut().remove(&from);
        func_data.dfg_mut().remove_value(from);
    }

    fn is_equal(kind: &ValueKind, other: &ValueKind) -> bool {
        match (kind, other) {
            (ValueKind::ZeroInit(_), ValueKind::ZeroInit(_)) => true,
            (ValueKind::Integer(i1), ValueKind::Integer(i2)) => i1.value() == i2.value(),
            (ValueKind::Load(l1), ValueKind::Load(l2)) => l1.src() == l2.src(),
            (ValueKind::Binary(b1), ValueKind::Binary(b2)) => {
                b1.op() == b2.op() && b1.lhs() == b2.lhs() && b1.rhs() == b2.rhs()
            }
            (ValueKind::GetElemPtr(g1), ValueKind::GetElemPtr(g2)) => {
                g1.src() == g2.src() && g1.index() == g2.index()
            }
            (ValueKind::GetPtr(g1), ValueKind::GetPtr(g2)) => {
                g1.src() == g2.src() && g1.index() == g2.index()
            }
            _ => false,
        }
    }
}
