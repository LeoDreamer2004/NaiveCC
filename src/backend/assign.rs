use super::flow::{FunctionFlowGraph, LiveVariableAnalyser, UseDefParser};
use super::frames::FrameStack;
use super::instruction::Inst;
use super::program::{AsmGlobal, AsmLocal};
use super::registers::{FakeRegister, Register, RegisterType};
use heuristic_graph_coloring::{color_greedy_dsatur, VecVecGraph};
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct RegisterAssigner;

/// Register dispatcher is used to dispatch registers for the values.
///
/// In [`AsmManager`], most of the values are stored in [`FakeRegister`] (which means we assumes
/// that there are infinite registers). They are called like "reg_1" etc.
/// This dispatcher is used to dispatch the registers for the values.
impl RegisterAssigner {
    pub fn assign(&mut self, asm: &mut AsmGlobal, fs: &mut FrameStack) {
        let mut flow = FunctionFlowGraph::default();
        flow.build(asm);
        let mut parser = UseDefParser::default();
        parser.parse(asm);
        let mut analyser = LiveVariableAnalyser::default();
        analyser.analyse(&flow, &parser);
        let mut graph = RegisterInterferenceGraph::default();
        graph.build(asm, &analyser);
        let mut rewriter = RegisterRewriter::new(graph);
        rewriter.save_on(asm, fs);
    }
}

type Color = usize;

struct RegisterInterferenceGraph {
    fake_regs: HashSet<usize>,
    graph: VecVecGraph,
    color_map: HashMap<usize, Color>,
}

impl Default for RegisterInterferenceGraph {
    fn default() -> Self {
        Self {
            fake_regs: HashSet::new(),
            graph: VecVecGraph::new(0),
            color_map: HashMap::new(),
        }
    }
}

impl RegisterInterferenceGraph {
    fn true_reg(&self, reg: &FakeRegister) -> Register {
        let color = self.color_map.get(&Self::to_idx(reg)).unwrap();
        // TODO: Maybe not only use Temp registers?
        RegisterType::all(&RegisterType::Temp)[color % 6]
    }

    fn conflict(&self, reg1: &FakeRegister, reg2: &FakeRegister) -> bool {
        self.color_map[&Self::to_idx(reg1)] == self.color_map[&Self::to_idx(reg2)]
    }

    fn build(&mut self, asm: &AsmGlobal, analyser: &LiveVariableAnalyser) {
        self.build_points(asm);
        self.build_edges(asm, analyser);
        self.color();
    }

    fn build_points(&mut self, asm: &AsmGlobal) {
        self.fake_regs.clear();
        self.color_map.clear();
        let mut max_idx = 0;

        // Scan all the fake registers and build the interference graph
        for (_, insts) in asm.labeled_locals() {
            for inst in insts {
                for reg in inst.regs() {
                    if let Register::Fake(r) = reg {
                        let idx = Self::to_idx(r);
                        if idx > max_idx {
                            max_idx = idx;
                        }
                        self.fake_regs.insert(idx);
                    }
                }
            }
        }
        self.graph = VecVecGraph::new(max_idx + 1);
    }

    fn build_edges(&mut self, asm: &AsmGlobal, lva: &LiveVariableAnalyser) {
        for (l, insts) in asm.labeled_locals() {
            let mut has_built = HashSet::new();
            for (i, inst) in insts.iter().enumerate() {
                if let Some(reg) = inst.dest_reg() {
                    if !matches!(reg, Register::Fake(_)) || !has_built.insert(reg) {
                        continue;
                    }
                    if lva.outs(l).contains(&reg) {
                        // if the register is an out-register

                        // all out-registers are interferences
                        for r in lva.outs(l) {
                            self.add_edge(&reg, &r);
                        }
                        // the next instructions are also interferences
                        for inst in insts.iter().skip(i) {
                            self.add_all_edges(&reg, inst);
                        }
                    } else {
                        // if the register is not an out-register

                        // here come interferences until the register is not used
                        let mut used = false;
                        for inst in insts.iter().skip(i).rev() {
                            if inst.src_regs().contains(&reg) {
                                used = true;
                            } else if inst.dest_reg() == Some(reg) {
                                used = false;
                            }
                            if used {
                                self.add_all_edges(&reg, inst);
                            }
                        }
                    }
                }
            }
        }
    }

    fn color(&mut self) {
        let color = color_greedy_dsatur(&self.graph);
        for reg_idx in &self.fake_regs {
            self.color_map.insert(*reg_idx, color[*reg_idx]);
        }
    }

    fn to_idx(reg: &FakeRegister) -> usize {
        reg.0
    }

    fn add_edge(&mut self, from: &Register, to: &Register) {
        if let Register::Fake(from) = from {
            if let Register::Fake(to) = to {
                if from != to {
                    self.graph.add_edge(Self::to_idx(from), Self::to_idx(to));
                }
            }
        }
    }

    fn add_all_edges(&mut self, from: &Register, to: &Inst) {
        for reg in to.regs() {
            self.add_edge(from, reg);
        }
    }
}

struct RegisterRewriter {
    graph: RegisterInterferenceGraph,
}

impl RegisterRewriter {
    fn new(graph: RegisterInterferenceGraph) -> Self {
        Self { graph }
    }

    fn save_on(&mut self, asm: &mut AsmGlobal, fs: &mut FrameStack) {
        let mut res = AsmGlobal::new_from(asm);
        for local in asm.locals() {
            let mut res_l = AsmLocal::new_from(local);
            for inst in local.insts() {
                let res_i = self.replace_reg(inst);
                res_l.insts_mut().extend(res_i);
            }
            res.new_local(res_l);
        }
        *asm = res;
    }

    fn replace_reg(&self, inst: &Inst) -> Vec<Inst> {
        let mut inst = inst.clone();
        for u_reg in inst.regs_mut() {
            if let Register::Fake(r) = u_reg {
                *u_reg = self.graph.true_reg(r);
            }
        }
        vec![inst]
    }
}
