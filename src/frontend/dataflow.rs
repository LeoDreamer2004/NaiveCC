use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default)]
pub struct FunctionFlowGraph {
    edges: HashMap<BasicBlock, Vec<BasicBlock>>,
}

impl FunctionFlowGraph {
    /// Builds the flow graph for the given function data
    ///
    /// # Arguments
    /// func_data: The function data
    pub fn build(&mut self, func_data: &FunctionData) {
        self.edges.clear();
        for (&bb, node) in func_data.layout().bbs() {
            let mut out = Vec::new();
            let &inst = node.insts().back_key().expect("Cannot be an empty block");
            match func_data.dfg().value(inst).kind() {
                ValueKind::Branch(branch) => {
                    out.push(branch.true_bb());
                    out.push(branch.false_bb());
                }
                ValueKind::Jump(jump) => {
                    out.push(jump.target());
                }
                ValueKind::Return(_) => {}
                _ => panic!("Invalid terminator instruction"),
            }
            self.edges.insert(bb, out);
        }
    }

    /// Returns all the basic blocks in the function
    pub fn bbs(&self) -> Vec<BasicBlock> {
        self.edges.keys().copied().collect()
    }

    /// Returns the next blocks reachable from the given block
    pub fn to(&self, block: BasicBlock) -> Vec<BasicBlock> {
        self.edges.get(&block).unwrap().clone()
    }

    /// Returns the previous blocks that can reach the given block
    pub fn from(&self, block: BasicBlock) -> Vec<BasicBlock> {
        self.edges
            .iter()
            .filter_map(|(&k, v)| v.contains(&block).then_some(k))
            .collect()
    }
}

#[derive(Debug, Default)]
pub struct GenKillParser {
    /// Generated set for each block
    gen: HashMap<BasicBlock, HashSet<Value>>,
    /// Killed set for each block
    kill: HashMap<BasicBlock, HashSet<Value>>,
}

impl GenKillParser {
    pub fn parse(&mut self, data: &FunctionData) {
        let mut def = HashMap::new();
        self.gen.clear();
        self.kill.clear();
        for (&bb, _) in data.layout().bbs() {
            self.gen.insert(bb, HashSet::new());
            self.kill.insert(bb, HashSet::new());
        }

        // All stores in the func: bb: [(inst, dest)]
        let mut stores = HashMap::new();

        // Scan all the stores in the function
        for (&bb, node) in data.layout().bbs() {
            for (&inst, _) in node.insts() {
                if let ValueKind::Store(store) = data.dfg().value(inst).kind() {
                    stores
                        .entry(bb)
                        .or_insert_with(Vec::new)
                        .push((inst, store.dest()));
                }
            }
        }

        // Compute all the definition
        for (_, pairs) in stores.clone() {
            for (inst, dest) in pairs {
                def.entry(dest).or_insert_with(HashSet::new).insert(inst);
            }
        }

        // Compute the gen and kill set
        for (bb, pairs) in stores {
            let mut gen_set = HashSet::new();
            let mut kill_set = HashSet::new();

            let mut inst_kill = HashMap::new();
            for (inst, dest) in pairs.clone() {
                let mut kills = def.get(&dest).unwrap().clone();
                assert!(kills.remove(&inst));
                inst_kill.insert(inst, kills);
            }

            for (inst, _) in pairs.into_iter().rev() {
                if !kill_set.contains(&inst) {
                    gen_set.insert(inst);
                }
                kill_set.extend(inst_kill.get(&inst).unwrap());
            }

            self.gen.insert(bb, gen_set);
            self.kill.insert(bb, kill_set);
        }
    }

    pub fn kill(&self, bb: BasicBlock) -> HashSet<Value> {
        self.kill.get(&bb).unwrap().clone()
    }

    pub fn gen(&self, bb: BasicBlock) -> HashSet<Value> {
        self.gen.get(&bb).unwrap().clone()
    }
}

#[derive(Debug, Default)]
pub struct ReachDefiniteAnalyser {
    ins: HashMap<BasicBlock, HashSet<Value>>,
    outs: HashMap<BasicBlock, HashSet<Value>>,
}

impl ReachDefiniteAnalyser {
    pub fn build(&mut self, graph: &FunctionFlowGraph, parser: &GenKillParser) {
        self.ins.clear();
        self.outs.clear();
        for bb in graph.bbs() {
            self.ins.insert(bb, HashSet::new());
            self.outs.insert(bb, HashSet::new());
        }

        let mut changed = true;
        while changed {
            changed = false;

            for bb in graph.bbs() {
                // IN[B] = U OUT[P]
                let mut in_set = HashSet::new();
                for pred in graph.from(bb) {
                    in_set.extend(self.outs[&pred].clone());
                }
                self.ins.insert(bb, in_set);

                // OUT[B] = gen[B] | (IN[B] - kill[B])
                let mut out_set = parser.gen(bb).clone();
                let in_set = self.ins[&bb].clone();
                let kill_set = parser.kill(bb);
                out_set.extend(in_set.difference(&kill_set));

                if out_set != self.outs[&bb] {
                    self.outs.insert(bb, out_set);
                    changed = true;
                }
            }
        }
    }

    pub fn ins(&self, bb: BasicBlock) -> &HashSet<Value> {
        self.ins.get(&bb).unwrap()
    }
}

#[derive(Debug, Default)]
pub struct EGenKillParser {
    u: HashSet<Value>,
    /// Generated set for each block
    e_gen: HashMap<BasicBlock, HashSet<Value>>,
    /// Killed set for each block
    e_kill: HashMap<BasicBlock, HashSet<Value>>,
}

impl EGenKillParser {
    pub fn update(inst: Value, availables: &mut HashSet<Value>, func_data: &FunctionData) {
        if func_data
            .dfg()
            .value(inst)
            .kind()
            .value_uses()
            .filter(|u| !func_data.dfg().values().contains_key(u))
            .count()
            > 0
        {
            return;
        }

        if !func_data.dfg().value(inst).ty().is_unit() {
            availables.insert(inst);
        } else if let ValueKind::Store(store) = func_data.dfg().value(inst).kind() {
            if !func_data.dfg().values().contains_key(&store.dest()) {
                return;
            }
            let used_by = func_data.dfg().value(store.dest()).used_by();
            availables.retain(|s| !used_by.contains(s));
        }
    }

    pub fn parse(&mut self, func_data: &FunctionData) {
        self.u.clear();
        self.e_gen.clear();
        self.e_kill.clear();

        for (&bb, node) in func_data.layout().bbs() {
            self.e_kill.insert(bb, HashSet::new());
            self.e_gen.insert(bb, HashSet::new());
            for (&inst, _) in node.insts() {
                if !func_data.dfg().value(inst).ty().is_unit() {
                    self.u.insert(inst);
                }
            }
        }

        for (&bb, node) in func_data.layout().bbs() {
            let gens = self.e_gen.get_mut(&bb).unwrap();
            for (&inst, _) in node.insts() {
                Self::update(inst, gens, func_data);
                if let ValueKind::Store(store) = func_data.dfg().value(inst).kind() {
                    if func_data.dfg().values().contains_key(&store.dest()) {
                        let used_by = func_data.dfg().value(store.dest()).used_by();
                        self.e_kill.get_mut(&bb).unwrap().extend(used_by);
                    }
                }
            }
        }
    }

    pub fn e_kill(&self, bb: BasicBlock) -> &HashSet<Value> {
        self.e_kill.get(&bb).unwrap()
    }

    pub fn e_gen(&self, bb: BasicBlock) -> &HashSet<Value> {
        self.e_gen.get(&bb).unwrap()
    }

    pub fn u(&self) -> &HashSet<Value> {
        &self.u
    }
}

#[derive(Debug, Default)]
pub struct AvailableExpressions {
    ins: HashMap<BasicBlock, HashSet<Value>>,
    outs: HashMap<BasicBlock, HashSet<Value>>,
}

impl AvailableExpressions {
    pub fn build(&mut self, graph: &FunctionFlowGraph, parser: &EGenKillParser) {
        self.ins.clear();
        self.outs.clear();
        for bb in graph.bbs() {
            self.ins.insert(bb, HashSet::new());
            self.outs.insert(bb, parser.u().clone());
        }

        let mut changed = true;
        while changed {
            changed = false;

            for bb in graph.bbs() {
                // IN[B] = \cap OUT[P]
                let mut in_set = parser.u().clone();
                for pred in graph.from(bb) {
                    in_set = in_set.intersection(&self.outs[&pred]).cloned().collect();
                }
                if graph.from(bb).is_empty() {
                    in_set = HashSet::new();
                }
                self.ins.insert(bb, in_set);

                // OUT[B] = gen[B] | (IN[B] - kill[B])
                let mut out_set = parser.e_gen(bb).clone();
                let in_set = self.ins[&bb].clone();
                let kill_set = parser.e_kill(bb);
                out_set.extend(in_set.difference(&kill_set));

                if out_set != self.outs[&bb] {
                    self.outs.insert(bb, out_set);
                    changed = true;
                }
            }
        }
    }

    pub fn ins(&self, bb: BasicBlock) -> &HashSet<Value> {
        self.ins.get(&bb).unwrap()
    }

    pub fn outs(&self, bb: BasicBlock) -> &HashSet<Value> {
        self.outs.get(&bb).unwrap()
    }
}
