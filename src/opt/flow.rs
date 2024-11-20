use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind};
use std::collections::{HashMap, HashSet};

type Inst = Value;

#[derive(Debug)]
pub struct FunctionFlowGraph {
    edges: HashMap<BasicBlock, Vec<BasicBlock>>,
}

impl FunctionFlowGraph {
    /// Builds the flow graph for the given function
    ///
    /// # Arguments
    /// func_data: The function data
    ///
    /// # Returns
    /// The flow graph, otherwise None (the function is a declaration)
    pub fn build(func_data: &FunctionData) -> Option<Self> {
        if func_data.layout().entry_bb().is_none() {
            return None;
        };

        let mut edges = HashMap::new();
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
            edges.insert(bb, out);
        }
        Some(Self { edges })
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

    /// Returns true if the given block is an entry block
    pub fn is_entry(&self, block: BasicBlock) -> bool {
        self.from(block).is_empty()
    }

    /// Returns true if the given block is an exit block
    pub fn is_exit(&self, block: BasicBlock) -> bool {
        self.to(block).is_empty()
    }
}

#[derive(Debug, Default)]
pub struct ReachDefiniteParser {
    /// All the definitions in the function
    def: HashMap<Value, HashSet<Inst>>,
    /// Generated set for each block
    gen: HashMap<BasicBlock, HashSet<Inst>>,
    /// Killed set for each block
    kill: HashMap<BasicBlock, HashSet<Inst>>,
}

impl ReachDefiniteParser {
    pub fn parse(data: &FunctionData) -> Self {
        let mut def = HashMap::new();
        let mut gen = HashMap::new();
        let mut kill = HashMap::new();
        for (&bb, _) in data.layout().bbs() {
            gen.insert(bb, HashSet::new());
            kill.insert(bb, HashSet::new());
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

        if stores.is_empty() {
            return Self::default();
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

            gen.insert(bb, gen_set);
            kill.insert(bb, kill_set);
        }

        Self { def, gen, kill }
    }

    pub fn kill(&self, bb: BasicBlock) -> HashSet<Inst> {
        self.kill.get(&bb).unwrap().clone()
    }

    pub fn gen(&self, bb: BasicBlock) -> HashSet<Inst> {
        self.gen.get(&bb).unwrap().clone()
    }
}

pub struct ControlFlowGraph {
    ins: HashMap<BasicBlock, HashSet<Inst>>,
    outs: HashMap<BasicBlock, HashSet<Inst>>,
}

impl ControlFlowGraph {
    pub fn build(graph: &FunctionFlowGraph, parser: &ReachDefiniteParser) -> Self {
        let mut ins = HashMap::new();
        let mut outs = HashMap::new();
        for bb in graph.bbs() {
            ins.insert(bb, HashSet::new());
            outs.insert(bb, HashSet::new());
        }

        let mut changed = true;
        while changed {
            changed = false;

            for bb in graph.bbs() {
                // IN[B] = U OUT[P]
                let mut in_set = HashSet::new();
                for pred in graph.from(bb) {
                    in_set.extend(outs[&pred].clone());
                }
                ins.insert(bb, in_set);

                // OUT[B] = gen[B] | (IN[B] - kill[B])
                let mut out_set = parser.gen(bb).clone();
                let in_set = ins[&bb].clone();
                let kill_set = parser.kill(bb);
                out_set.extend(in_set.difference(&kill_set));

                if out_set != outs[&bb] {
                    outs.insert(bb, out_set);
                    changed = true;
                }
            }
        }

        Self { ins, outs }
    }
}

pub struct LiveVariableAnalyser {
    ins: HashMap<BasicBlock, HashSet<Inst>>,
    outs: HashMap<BasicBlock, HashSet<Inst>>,
}

impl LiveVariableAnalyser {
    pub fn anaylsis(graph: &FunctionFlowGraph, parser: &ReachDefiniteParser) -> Self {
        let mut ins = HashMap::new();
        let mut outs = HashMap::new();

        for bb in graph.bbs() {
            ins.insert(bb, HashSet::new());
            outs.insert(bb, HashSet::new());
        }

        let mut changed = true;
        while changed {
            changed = false;
            for bb in graph.bbs() {
                // OUT[B] = U IN[S]
                let mut out_set = HashSet::new();
                for succ in graph.to(bb) {
                    out_set.extend(ins[&succ].clone());
                }
                outs.insert(bb, out_set);

                // IN[B] = gen[B] | (OUT[B] - kill[B])
                let mut in_set = parser.gen(bb).clone();
                let out_set = outs[&bb].clone();
                let kill_set = parser.kill(bb);
                in_set.extend(out_set.difference(&kill_set));

                if in_set != ins[&bb] {
                    ins.insert(bb, in_set);
                    changed = true;
                }
            }
        }

        Self { ins, outs }
    }
}
