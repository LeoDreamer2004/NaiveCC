use koopa::ir::{BasicBlock, FunctionData, Value, ValueKind};
use std::collections::{HashMap, HashSet};

type Inst = Value;

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
    /// Generated set for each block
    gen: HashMap<BasicBlock, HashSet<Inst>>,
    /// Killed set for each block
    kill: HashMap<BasicBlock, HashSet<Inst>>,
}

impl ReachDefiniteParser {
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

        if stores.is_empty() {
            return;
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

    pub fn kill(&self, bb: BasicBlock) -> HashSet<Inst> {
        self.kill.get(&bb).unwrap().clone()
    }

    pub fn gen(&self, bb: BasicBlock) -> HashSet<Inst> {
        self.gen.get(&bb).unwrap().clone()
    }
}

#[derive(Debug, Default)]
pub struct ControlFlowGraph {
    ins: HashMap<BasicBlock, HashSet<Inst>>,
    outs: HashMap<BasicBlock, HashSet<Inst>>,
}

impl ControlFlowGraph {
    pub fn build(&mut self, graph: &FunctionFlowGraph, parser: &ReachDefiniteParser) {
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
}
