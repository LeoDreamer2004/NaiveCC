//! Register assignment module.

use super::address::Stack;
use super::dataflow::{GlobalFLowGraph, LiveVariableAnalyser, UseDefParser};
use super::frames::StackFrame;
use super::instruction::Inst;
use super::program::{AsmGlobal, AsmLocal};
use super::registers::{FakeRegister, Register, RegisterType};
use super::INT_SIZE;
use heuristic_graph_coloring::{color_rlf, VecVecGraph};
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct RegisterAssigner;

/// Register dispatcher is used to dispatch registers for the values.
///
/// In [`ValueTable`], most of the values are stored in [`FakeRegister`] (which means we assumes
/// that there are infinite registers). They are called like "reg_1" etc.
/// This dispatcher is used to dispatch the registers for the values.
impl RegisterAssigner {
    pub fn assign(&mut self, asm: &mut AsmGlobal, sf: &mut StackFrame) {
        let mut ok = false;
        loop {
            let mut flow = GlobalFLowGraph::default();
            flow.build(asm);
            let mut parser = UseDefParser::default();
            parser.parse(asm);
            let mut analyser = LiveVariableAnalyser::new(flow);
            analyser.analyse(&parser);
            let mut graph = RegisterInterferenceGraph::default();
            graph.build(asm, &analyser);
            if ok {
                let mut writer = RegisterRewriter::new_on(graph);
                writer.write(asm, sf, analyser);
                break;
            } else {
                let mut allocator = MemoryAllocator::default();
                ok = allocator.alloc(asm, sf, &graph);
            }
        }
    }
}

type Color = usize;

/// Register interference graph (RIG) is used to represent the interference between the registers.
///
/// We first consider there are infinite registers,
/// When two registers are used at the same time, they are considered to be in conflict,
/// and then these two registers are given different colors.
/// The goal is to minimize the number of colors used.
struct RegisterInterferenceGraph {
    fake_regs: HashSet<usize>,
    graph: VecVecGraph,
    color_map: HashMap<usize, Color>,
    good_colors: Vec<Color>,
}

impl Default for RegisterInterferenceGraph {
    fn default() -> Self {
        Self {
            fake_regs: HashSet::new(),
            graph: VecVecGraph::new(0),
            color_map: HashMap::new(),
            good_colors: Vec::new(),
        }
    }
}

impl RegisterInterferenceGraph {
    const COLOR_NUM: usize = 17;

    fn real_reg(&self, reg: &FakeRegister) -> Register {
        let color = *self.color_map.get(&Self::to_idx(reg)).unwrap();
        let idx = self.color_idx(color).expect("Bad color");
        RegisterType::Temp.all()[idx]
    }

    fn has_space(&self, reg: &FakeRegister) -> bool {
        self.good_colors
            .contains(self.color_map.get(&Self::to_idx(reg)).unwrap())
    }

    /// Build the interference graph when live analysis is done.
    fn build(&mut self, asm: &AsmGlobal, analyser: &LiveVariableAnalyser) {
        self.build_points(asm);
        self.build_edges(asm, analyser);
        self.color();
    }

    fn fake_regs(&self) -> &HashSet<usize> {
        &self.fake_regs
    }

    fn build_points(&mut self, asm: &AsmGlobal) {
        self.fake_regs.clear();
        self.color_map.clear();
        let mut max_idx = 0;

        // Scan all the fake registers and build the interference graph
        for (_, local) in asm.labeled_locals() {
            for inst in local.insts() {
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

    fn build_edges(&mut self, asm: &AsmGlobal, analyser: &LiveVariableAnalyser) {
        for (l, local) in asm.labeled_locals() {
            let mut active_set = analyser.outs(&l).clone();
            for inst in local.insts().iter().rev() {
                if let Some(reg) = inst.dest_reg() {
                    active_set.remove(&reg);
                    for r in &active_set {
                        self.add_edge(reg, r);
                    }
                }
                for reg in inst.src_regs() {
                    active_set.insert(*reg);
                }
            }
            assert_eq!(&active_set, analyser.ins(&l));
        }
    }

    fn color(&mut self) {
        let color = color_rlf(&self.graph);
        for &reg_idx in &self.fake_regs {
            self.color_map.insert(reg_idx, color[reg_idx]);
        }
        self.choose_best_color(color);
    }

    fn choose_best_color(&mut self, color: Vec<Color>) {
        // greedily choose the most frequent colors
        let color_count = color.iter().fold(HashMap::new(), |mut acc, x| {
            *acc.entry(x).or_insert(0) += 1;
            acc
        });
        let mut color_count: Vec<_> = color_count.into_iter().collect();
        color_count.sort_by_key(|(_, v)| -(*v));
        self.good_colors = color_count
            .iter()
            .take(Self::COLOR_NUM)
            .map(|(k, _)| **k)
            .collect();

        // extend the good colors to the size of the color
        for _ in 0..(Self::COLOR_NUM as i32 - self.good_colors.len() as i32) {
            self.good_colors.push(0);
        }
    }

    fn color_idx(&self, color: Color) -> Option<usize> {
        self.good_colors.iter().position(|&c| c == color)
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
}

/// Naive memory allocator when register assignment fails.
#[derive(Default)]
struct MemoryAllocator {
    loc_map: HashMap<FakeRegister, Stack>,
    reg_idx: usize,
}

impl MemoryAllocator {
    /// Allocate memory for those values that are not in registers.
    ///
    /// In [`RegisterInterferenceGraph`],
    /// when the number of color is more than the predefined number,
    /// allocate memory for the values that cannot be happy in registers.
    pub fn alloc(
        &mut self,
        asm: &mut AsmGlobal,
        sf: &mut StackFrame,
        graph: &RegisterInterferenceGraph,
    ) -> bool {
        self.reg_idx = graph.fake_regs().iter().max().unwrap_or(&0) + 1;

        let mut res = AsmGlobal::new_from(asm);
        let mut ok = true;
        for local in asm.locals() {
            let mut res_l = AsmLocal::new_from(local);
            for inst in local.insts() {
                let mut res_inst = inst.clone();
                let dest = inst.dest_reg().cloned();
                let mut save_inst = None;
                for reg in res_inst.regs_mut() {
                    let is_dest = Some(*reg) == dest;
                    if let Register::Fake(fake) = reg {
                        if !graph.has_space(fake) {
                            ok = false;

                            if is_dest {
                                // save the register to the stack
                                save_inst = Some(self.save_to_stack(fake.clone(), sf));
                            } else {
                                // load the register from the stack
                                let (r, i) = self.load_from_stack(fake, sf);
                                *reg = r;
                                res_l.insts_mut().push(i);
                            }
                        }
                    }
                }
                res_l.insts_mut().push(res_inst);
                if let Some(inst) = save_inst {
                    res_l.insts_mut().push(inst);
                }
            }
            res.new_local(res_l);
        }
        *asm = res;

        ok
    }

    fn new_reg(&mut self) -> Register {
        let reg = Register::Fake(FakeRegister(self.reg_idx));
        self.reg_idx += 1;
        reg
    }

    fn save_to_stack(&mut self, fake: FakeRegister, sf: &mut StackFrame) -> Inst {
        let loc = self.get_alloc(&fake, sf);
        Inst::Sw(Register::Fake(fake), loc.base, loc.offset)
    }

    fn load_from_stack(&mut self, fake: &FakeRegister, sf: &mut StackFrame) -> (Register, Inst) {
        let reg = self.new_reg();
        let loc = self.get_alloc(fake, sf);
        (reg, Inst::Lw(reg, loc.base, loc.offset))
    }

    fn get_alloc(&mut self, fake: &FakeRegister, sf: &mut StackFrame) -> Stack {
        match self.loc_map.get(fake) {
            Some(loc) => loc.clone(),
            None => {
                let loc = sf.malloc(INT_SIZE).unwrap();
                self.loc_map.insert(*fake, loc.clone());
                loc
            }
        }
    }
}

/// We are using [`FakeRegister`] up to now,
/// and this rewriter is used to rewrite the registers to the real registers.
struct RegisterRewriter {
    graph: RegisterInterferenceGraph,
    loc_map: HashMap<Register, Stack>,
}

impl RegisterRewriter {
    fn new_on(graph: RegisterInterferenceGraph) -> Self {
        Self {
            graph,
            loc_map: HashMap::new(),
        }
    }

    /// Write the registers to the real registers.
    ///
    /// Note: May add some instructions to save and load the registers to the stack
    /// when going through blocks / meeting function calls.
    fn write(&mut self, asm: &mut AsmGlobal, sf: &mut StackFrame, analyser: LiveVariableAnalyser) {
        for (label, local) in asm.labeled_locals_mut() {
            let mut res = AsmLocal::new_from(local);
            let insts = res.insts_mut();

            let mut used_set = HashSet::new();
            let mut def_ref_cnt = HashMap::new();
            let mut active_set = analyser.outs(&label).clone();
            for inst in local.insts() {
                if let Some(dest) = inst.dest_reg() {
                    *def_ref_cnt.entry(dest.clone()).or_insert(0) += 1;
                }
            }

            // Do everying in reverse order!
            for inst in local.insts_mut().iter_mut().rev() {
                // Update the active set
                for reg in inst.regs() {
                    active_set.insert(reg.clone());
                }
                if let Some(reg) = inst.dest_reg() {
                    *def_ref_cnt.get_mut(reg).unwrap() -= 1;
                    used_set.remove(reg);
                    active_set.remove(&reg);
                }
                for reg in inst.src_regs() {
                    used_set.insert(reg.clone());
                }

                // Recover the registers when meet a call
                if let Inst::Call(_) = inst {
                    for reg in &active_set {
                        if let Some(inst) = self.load_from_stack(reg.clone(), sf) {
                            insts.push(inst);
                        }
                    }
                }

                // Rewrite the registers
                insts.push(self.update_inst(inst));

                // Save the registers before the jump / call
                let empty = &HashSet::new();
                let to_store = match inst {
                    Inst::J(l) => analyser.ins(l),
                    Inst::Beqz(_, l) => analyser.ins(l),
                    Inst::Bnez(_, l) => analyser.ins(l),
                    Inst::Call(_) => &active_set,
                    _ => empty,
                };

                for reg in to_store {
                    if *def_ref_cnt.get(reg).unwrap_or(&0) > 0 {
                        if let Some(inst) = self.save_to_stack(*reg, sf) {
                            insts.push(inst);
                        }
                    }
                }
            }

            // When the block only comes from a single block, we can directly use the registers
            if analyser.flow().from_duplicate(&label).len() != 1 {
                for reg in used_set {
                    if let Some(inst) = self.load_from_stack(reg, sf) {
                        insts.push(inst);
                    }
                }
            }

            insts.reverse();
            *local = res;
        }
    }

    fn update_inst(&mut self, inst: &Inst) -> Inst {
        let mut inst = inst.clone();
        for u_reg in inst.regs_mut() {
            if let Register::Fake(fake) = u_reg {
                let real = self.graph.real_reg(fake);
                *u_reg = real;
            }
        }
        inst
    }

    fn load_from_stack(&mut self, reg: Register, sf: &mut StackFrame) -> Option<Inst> {
        let real = match &reg {
            Register::Fake(fake) => self.graph.real_reg(fake),
            _ => return None,
        };
        let loc = self.get_alloc(&reg, sf);
        Some(Inst::Lw(real, loc.base, loc.offset))
    }

    fn save_to_stack(&mut self, reg: Register, sf: &mut StackFrame) -> Option<Inst> {
        let real = match &reg {
            Register::Fake(fake) => self.graph.real_reg(fake),
            _ => return None,
        };
        let loc = self.get_alloc(&reg, sf);
        Some(Inst::Sw(real, loc.base, loc.offset))
    }

    fn get_alloc(&mut self, reg: &Register, sf: &mut StackFrame) -> Stack {
        match self.loc_map.get(reg) {
            Some(loc) => loc.clone(),
            None => {
                let loc = sf.malloc(INT_SIZE).unwrap();
                self.loc_map.insert(reg.clone(), loc.clone());
                loc
            }
        }
    }
}
