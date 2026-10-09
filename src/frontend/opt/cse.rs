use super::super::dataflow::{AvailableExpressions, EGenKillParser, FunctionFlowGraph};
use super::util::ValueReplace;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Value, ValueKind};
use koopa::opt::FunctionPass;
use std::collections::{HashMap, HashSet};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Expression {
    Load(Value),
    Binary(BinaryOp, Value, Value),
    GetElemPtr(Value, Value),
    GetPtr(Value, Value),
}

impl Expression {
    fn from_kind(kind: &ValueKind) -> Option<Self> {
        match kind {
            ValueKind::Load(load) => Some(Self::Load(load.src())),
            ValueKind::Binary(binary) => {
                Some(Self::Binary(binary.op(), binary.lhs(), binary.rhs()))
            }
            ValueKind::GetElemPtr(ptr) => Some(Self::GetElemPtr(ptr.src(), ptr.index())),
            ValueKind::GetPtr(ptr) => Some(Self::GetPtr(ptr.src(), ptr.index())),
            _ => None,
        }
    }
}

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
        Self::merge_int(func_data);
        self.graph.build(func_data);
        while let Some((bb, from, to)) = self.scan(func_data) {
            Self::eliminate(func_data, bb, from, to);
        }
    }
}

impl CommonSubexpression {
    fn merge_int(func_data: &mut FunctionData) {
        // int -> value
        let mut availables = HashMap::new();
        // replace value -> value
        let mut worklist = Vec::new();
        for (&value, data) in func_data.dfg().values() {
            if let ValueKind::Integer(int) = data.kind() {
                match availables.get(&int.value()) {
                    Some(&v) => {
                        worklist.push((value, v));
                    }
                    None => {
                        availables.insert(int.value(), value);
                    }
                }
            }
        }
        for (from, to) in worklist {
            func_data.replace_value(from, to);
        }
    }

    fn scan(&self, func_data: &FunctionData) -> Option<(BasicBlock, Value, Value)> {
        let mut parser = EGenKillParser::default();
        parser.parse(func_data);
        let mut analyser = AvailableExpressions::default();
        analyser.build(&self.graph, &parser);

        for (&bb, node) in func_data.layout().bbs() {
            let mut availables = analyser.ins(bb).clone();
            let mut expressions: HashMap<Expression, HashSet<Value>> = HashMap::new();
            for &value in &availables {
                if let Some(expr) = Expression::from_kind(func_data.dfg().value(value).kind()) {
                    expressions.entry(expr).or_default().insert(value);
                }
            }
            for &inst in node.insts().keys() {
                let data = func_data.dfg().value(inst);
                if let Some(expr) = Expression::from_kind(data.kind()) {
                    if let Some(values) = expressions.get(&expr) {
                        if let Some(&value) = values.iter().find(|&&value| value != inst) {
                            return Some((bb, inst, value));
                        }
                    }
                    expressions.entry(expr).or_default().insert(inst);
                } else if let ValueKind::Store(store) = data.kind() {
                    let users: Vec<_> =
                        if let Some(dest) = func_data.dfg().values().get(&store.dest()) {
                            dest.used_by().iter().copied().collect()
                        } else {
                            availables
                                .iter()
                                .copied()
                                .filter(|&value| {
                                    func_data
                                        .dfg()
                                        .value(value)
                                        .kind()
                                        .value_uses()
                                        .any(|value| value == store.dest())
                                })
                                .collect()
                        };
                    for user in users {
                        if let Some(expr) =
                            Expression::from_kind(func_data.dfg().value(user).kind())
                        {
                            if let Some(values) = expressions.get_mut(&expr) {
                                values.remove(&user);
                                if values.is_empty() {
                                    expressions.remove(&expr);
                                }
                            }
                        }
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
}
