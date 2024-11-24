use super::instruction::{Inst, Label};
use super::program::AsmGlobal;
use super::registers::Register;
use std::collections::{HashMap, HashSet};

struct AsmSerializer<'a> {
    asm: &'a AsmGlobal,
    bias: HashMap<Label, usize>,
}

impl<'a> AsmSerializer<'a> {
    fn build(asm: &'a AsmGlobal) -> Self {
        let mut bias = HashMap::new();
        let mut count = 0;
        for local in asm.locals() {
            if let Some(l) = local.label() {
                let t = bias.insert(l.clone(), count);
                assert!(t.is_none(), "Duplicate label");
                count += local.insts().len();
            }
        }
        Self { asm, bias }
    }

    fn index(&self, label: &Label, bias: usize) -> usize {
        bias + self.bias.get(label).unwrap()
    }

    fn inst(&self, index: usize) -> Inst {
        let mut count = 0;
        for local in self.asm.locals() {
            if let Some(l) = local.label() {
                let len = local.insts().len();
                if count + len > index {
                    return local.insts()[index - count].clone();
                }
                count += len;
            }
        }
        unreachable!()
    }
}

#[derive(Default, Debug)]
pub struct FunctionFlowGraph {
    edges: HashMap<Label, Vec<Label>>,
}

impl FunctionFlowGraph {
    /// Builds the flow graph for the given function
    ///
    /// # Arguments
    /// func_data: The function data
    ///
    /// # Returns
    /// The flow graph, otherwise None (the function is a declaration)
    pub fn build(&mut self, asm: &AsmGlobal) {
        for (i, local) in asm.locals().iter().enumerate() {
            if let Some(l) = local.label() {
                let mut out = Vec::new();
                let mut flag = true;
                for inst in local.insts() {
                    match inst {
                        Inst::Beqz(_, t) => out.push(t.clone()),
                        Inst::Bnez(_, t) => out.push(t.clone()),
                        Inst::J(t) => {
                            out.push(t.clone());
                            flag = false;
                            break;
                        }
                        Inst::Ret => {
                            flag = false;
                            break;
                        }
                        _ => {}
                    }
                }

                // Go to the next block
                if flag {
                    if let Some(l) = asm.locals().get(i + 1) {
                        if let Some(l) = l.label() {
                            out.push(l.clone());
                        }
                    } // OUT[B] = U IN[S]
                }

                self.edges.insert(l.clone(), out);
            }
        }
    }

    /// Returns all the labels in the function
    pub fn labels(&self) -> Vec<&Label> {
        self.edges.keys().collect()
    }

    /// Returns the next blocks reachable from the given block
    pub fn to(&self, block: &Label) -> &Vec<Label> {
        self.edges.get(block).unwrap()
    }

    /// Returns the previous blocks that can reach the given block
    pub fn from(&self, block: &Label) -> Vec<&Label> {
        self.edges
            .iter()
            .filter_map(|(k, v)| v.contains(block).then_some(k))
            .collect()
    }

    /// Returns true if the given block is an entry block
    pub fn is_entry(&self, block: &Label) -> bool {
        self.from(block).is_empty()
    }

    /// Returns true if the given block is an exit block
    pub fn is_exit(&self, block: &Label) -> bool {
        self.to(block).is_empty()
    }
}

macro_rules! debug_insts {
    ($map:expr, $ser:expr) => {
        println!(stringify!($map));
        for (label, set) in $map {
            println!(
                "{}: {:?}",
                label,
                set.iter().map(|i| $ser.inst(*i)).collect::<Vec<_>>()
            );
        }
    };
}

#[derive(Debug, Default)]
pub struct GenKillParser {
    /// Generated set for each block
    gen: HashMap<Label, HashSet<usize>>,
    /// Killed set for each block
    kill: HashMap<Label, HashSet<usize>>,
}

impl GenKillParser {
    pub fn parse(asm: &AsmGlobal) -> Self {
        let ser = AsmSerializer::build(asm);

        let mut def = HashMap::new();
        let mut gen = HashMap::new();
        let mut kill = HashMap::new();

        for local in asm.locals() {
            if let Some(l) = local.label() {
                gen.insert(l.clone(), HashSet::new());
                kill.insert(l.clone(), HashSet::new());
            }
        }

        // All inst that can change register in the func: label: [(inst, dest)]
        let mut insts = HashMap::new();

        for local in asm.locals() {
            if let Some(l) = local.label() {
                let mut temp = Vec::new();
                for (i, inst) in local.insts().iter().enumerate() {
                    if let Some(reg) = inst.dest_reg() {
                        temp.push((ser.index(l, i), reg));
                    }
                }
                insts.insert(l.clone(), temp);
            }
        }

        // Compute all the definition
        for (_, pairs) in insts.clone() {
            for (inst, dest) in pairs {
                def.entry(dest).or_insert_with(HashSet::new).insert(inst);
            }
        }

        if insts.is_empty() {
            return Self::default();
        }

        // Compute the gen and kill set
        for (l, pairs) in insts {
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
                    gen_set.insert(inst.clone());
                }
                kill_set.extend(inst_kill.get(&inst).unwrap().clone());
            }

            gen.insert(l.clone(), gen_set);
            kill.insert(l.clone(), kill_set);
        }

        // debug_insts!(&gen, ser);
        Self { gen, kill }
    }

    pub fn kill(&self, label: &Label) -> HashSet<usize> {
        self.kill.get(label).unwrap().clone()
    }

    pub fn gen(&self, label: &Label) -> HashSet<usize> {
        self.gen.get(label).unwrap().clone()
    }
}

// pub struct ControlFlowGraph {
//     ins: HashMap<BasicBlock, HashSet<Inst>>,
//     outs: HashMap<BasicBlock, HashSet<Inst>>,
// }

// impl ControlFlowGraph {
//     pub fn build(graph: &FunctionFlowGraph, parser: &ReachDefiniteParser) -> Self {
//         let mut ins = HashMap::new();
//         let mut outs = HashMap::new();
//         for bb in graph.bbs() {
//             ins.insert(bb, HashSet::new());
//             outs.insert(bb, HashSet::new());
//         }

//         let mut changed = true;
//         while changed {
//             changed = false;

//             for bb in graph.bbs() {
//                 // IN[B] = U OUT[P]
//                 let mut in_set = HashSet::new();
//                 for pred in graph.from(bb) {
//                     in_set.extend(outs[&pred].clone());
//                 }
//                 ins.insert(bb, in_set);

//                 // OUT[B] = gen[B] | (IN[B] - kill[B])
//                 let mut out_set = parser.gen(bb).clone();
//                 let in_set = ins[&bb].clone();
//                 let kill_set = parser.kill(bb);
//                 out_set.extend(in_set.difference(&kill_set));

//                 if out_set != outs[&bb] {
//                     outs.insert(bb, out_set);
//                     changed = true;
//                 }
//             }
//         }

//         Self { ins, outs }
//     }
// }
#[derive(Debug, Default)]
pub struct UseDefParser {
    uses: HashMap<Label, HashSet<Register>>,
    defs: HashMap<Label, HashSet<Register>>,
}

impl UseDefParser {
    pub fn parse(&mut self, asm: &AsmGlobal) {
        self.uses.clear();
        self.defs.clear();

        for local in asm.locals() {
            if let Some(l) = local.label() {
                self.uses.insert(l.clone(), HashSet::new());
                self.defs.insert(l.clone(), HashSet::new());
            }
        }

        for local in asm.locals() {
            if let Some(l) = local.label() {
                let mut use_set = HashSet::new();
                let mut def_set = HashSet::new();

                for inst in local.insts() {
                    for reg in inst.src_regs() {
                        if !def_set.contains(reg) {
                            use_set.insert(reg.clone());
                        }
                    }
                    if let Some(reg) = inst.dest_reg() {
                        def_set.insert(reg.clone());
                    }
                }

                self.uses.insert(l.clone(), use_set);
                self.defs.insert(l.clone(), def_set);
            }
        }
    }

    pub fn use_(&self, label: &Label) -> HashSet<Register> {
        self.uses.get(label).unwrap().clone()
    }

    pub fn def(&self, label: &Label) -> HashSet<Register> {
        self.defs.get(label).unwrap().clone()
    }
}

#[derive(Debug)]
pub struct LiveVariableAnalyser {
    flow: FunctionFlowGraph,
    ins: HashMap<Label, HashSet<Register>>,
    outs: HashMap<Label, HashSet<Register>>,
}

impl LiveVariableAnalyser {
    pub fn new(flow: FunctionFlowGraph) -> Self {
        Self {
            flow,
            ins: HashMap::new(),
            outs: HashMap::new(),
        }
    }

    pub fn analyse(&mut self, parser: &UseDefParser) {
        self.ins.clear();
        self.outs.clear();

        for label in self.flow.labels() {
            self.ins.insert(label.clone(), HashSet::new());
            self.outs.insert(label.clone(), HashSet::new());
        }

        let mut changed = true;
        while changed {
            changed = false;
            for label in self.flow.labels() {

                // OUT[B] = U IN[S]
                let mut out_set = HashSet::new();
                for succ in self.flow.to(label) {
                    out_set.extend(self.ins[succ].clone());
                }
                self.outs.insert(label.clone(), out_set);

                // IN[B] = use[B] | (OUT[B] - def[B])
                let mut in_set = parser.use_(label);
                let out_set = self.outs[label].clone();
                let def_set = parser.def(label);
                for inst in out_set.difference(&def_set) {
                    in_set.insert(inst.clone());
                }

                if in_set != self.ins[label] {
                    self.ins.insert(label.clone(), in_set);
                    changed = true;
                }
            }
        }
    }

    pub fn ins(&self, label: &Label) -> &HashSet<Register> {
        self.ins.get(label).unwrap()
    }

    pub fn outs(&self, label: &Label) -> &HashSet<Register> {
        self.outs.get(label).unwrap()
    }
}
