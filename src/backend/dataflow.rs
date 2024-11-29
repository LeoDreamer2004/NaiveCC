//! Data Flow Analysis for optimization.

use super::instruction::{Inst, Label};
use super::program::AsmGlobal;
use super::registers::Register;
use std::collections::{HashMap, HashSet};

/// Flow graph for a [`AsmGlobal`].
///
/// A directed graph, where each point is the label of a basic block.
#[derive(Default, Debug)]
pub struct GlobalFLowGraph {
    edges: Vec<(Label, Label)>,
    all_labels: HashSet<Label>,
}

impl GlobalFLowGraph {
    /// Builds the flow graph for the given function
    ///
    /// # Arguments
    /// func_data: The function data
    ///
    /// # Returns
    /// The flow graph, otherwise None (the function is a declaration)
    pub fn build(&mut self, asm: &AsmGlobal) {
        self.all_labels = asm
            .locals()
            .iter()
            .filter_map(|l| l.label().clone())
            .collect();

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
                        } // OUT[B] = U IN[S]
                        _ => {}
                    }
                }

                // Go to the next block
                if flag {
                    if let Some(l) = asm.locals().get(i + 1) {
                        if let Some(l) = l.label() {
                            out.push(l.clone());
                        }
                    }
                }

                for o in out {
                    self.edges.push((l.clone(), o));
                }
            }
        }
    }

    /// Returns all the labels in the function
    pub fn labels(&self) -> &HashSet<Label> {
        &self.all_labels
    }

    /// Returns the next blocks reachable from the given block
    pub fn to(&self, block: &Label) -> HashSet<&Label> {
        let mut labels = HashSet::new();
        for (from, to) in &self.edges {
            if from == block {
                labels.insert(to);
            }
        }
        labels
    }

    pub fn to_duplicate(&self, block: &Label) -> Vec<&Label> {
        let mut labels = Vec::new();
        for (from, to) in &self.edges {
            if from == block {
                labels.push(to);
            }
        }
        labels
    }

    #[allow(dead_code)]
    /// Returns the previous blocks that can reach the given block
    pub fn from(&self, block: &Label) -> HashSet<&Label> {
        let mut labels = HashSet::new();
        for (from, to) in &self.edges {
            if to == block {
                labels.insert(from);
            }
        }
        labels
    }

    /// Returns the previous blocks that can reach the given block
    pub fn from_duplicate(&self, block: &Label) -> Vec<&Label> {
        let mut labels = Vec::new();
        for (from, to) in &self.edges {
            if to == block {
                labels.push(from);
            }
        }
        labels
    }
}

/// Use-Def parser for [`AsmGlobal`].
#[derive(Debug, Default)]
pub struct UseDefParser {
    uses: HashMap<Label, HashSet<Register>>,
    defs: HashMap<Label, HashSet<Register>>,
}

impl UseDefParser {
    /// Computes the **use** and **def** set for each block:
    /// - def: Registers that are defined in the block
    /// - use: Registers that are used (possibly before defined) in the block
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

/// Analyser for live variables.
///
/// Read flow from [`FunctionFlowGraph`] and use-def information from [`UseDefParser`].
#[derive(Debug)]
pub struct LiveVariableAnalyser {
    flow: GlobalFLowGraph,
    ins: HashMap<Label, HashSet<Register>>,
    outs: HashMap<Label, HashSet<Register>>,
}

impl LiveVariableAnalyser {
    pub fn new(flow: GlobalFLowGraph) -> Self {
        Self {
            flow,
            ins: HashMap::new(),
            outs: HashMap::new(),
        }
    }

    /// Compute the **in** and **out** set for each block:
    /// - in: Registers should be used at the beginning of the block
    /// - out: Registers that may be used later at the end of the block
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
