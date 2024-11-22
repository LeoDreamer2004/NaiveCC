use super::instruction::{Directive, Inst, Label};
use crate::utils::namer::UniqueNameGenerator;
use std::io;

// pub type AsmProgram = Vec<Inst>;
pub struct AsmProgram {
    globals: Vec<AsmGlobal>,
}

#[derive(Debug, Clone)]
pub struct AsmGlobal {
    local_namer: UniqueNameGenerator,
    directive: Directive,
    label: Label,
    locals: Vec<AsmLocal>,
}

#[derive(Debug, Clone)]
pub struct AsmLocal {
    label: Option<Label>,
    insts: Vec<Inst>,
}

impl AsmGlobal {
    pub fn new(directive: Directive, label: Label) -> Self {
        AsmGlobal {
            local_namer: UniqueNameGenerator::default(),
            directive,
            label,
            locals: Vec::new(),
        }
    }

    pub fn new_from(other: &AsmGlobal) -> Self {
        Self::new(other.directive().clone(), other.label().clone())
    }

    pub fn directive(&self) -> &Directive {
        &self.directive
    }

    pub fn label(&self) -> &Label {
        &self.label
    }

    pub fn locals(&self) -> &Vec<AsmLocal> {
        &self.locals
    }

    pub fn labeled_locals(&self) -> Vec<(&Label, &Vec<Inst>)> {
        self.locals
            .iter()
            .filter_map(|l| l.label().as_ref().map(|label| (label, l.insts())))
            .collect()
    }

    pub fn locals_mut(&mut self) -> &mut Vec<AsmLocal> {
        &mut self.locals
    }

    pub fn new_local(&mut self, mut local: AsmLocal) {
        local.label = local.label.map(|l| self.local_namer.get_name(l));
        self.locals.push(local);
    }

    pub fn find_local(&self, label: &Label) -> Option<&AsmLocal> {
        for local in self.locals() {
            if let Some(l) = local.label() {
                if l == label {
                    return Some(local);
                }
            }
        }
        None
    }
}

impl AsmLocal {
    pub fn new(label: Option<Label>) -> Self {
        AsmLocal {
            label,
            insts: Vec::new(),
        }
    }

    pub fn new_from(other: &AsmLocal) -> Self {
        Self::new(other.label().clone())
    }

    pub fn label(&self) -> &Option<Label> {
        &self.label
    }

    pub fn insts(&self) -> &Vec<Inst> {
        &self.insts
    }

    pub fn insts_mut(&mut self) -> &mut Vec<Inst> {
        &mut self.insts
    }
}

impl AsmProgram {
    pub fn new() -> Self {
        AsmProgram {
            globals: Vec::new(),
        }
    }

    pub fn globals(&self) -> &Vec<AsmGlobal> {
        &self.globals
    }

    pub fn globals_mut(&mut self) -> &mut Vec<AsmGlobal> {
        &mut self.globals
    }

    pub fn new_global(&mut self, glb: AsmGlobal) {
        self.globals.push(glb);
    }

    pub fn push(&mut self, inst: Inst) {
        self.cur_local_mut().insts_mut().push(inst);
    }

    pub fn cur_local(&self) -> &AsmLocal {
        self.cur_global().locals().last().unwrap()
    }

    pub fn cur_local_mut(&mut self) -> &mut AsmLocal {
        self.cur_global_mut().locals_mut().last_mut().unwrap()
    }

    pub fn cur_global(&self) -> &AsmGlobal {
        self.globals().last().unwrap()
    }

    pub fn cur_global_mut(&mut self) -> &mut AsmGlobal {
        self.globals_mut().last_mut().unwrap()
    }

    pub fn emit(&self, mut output: impl io::Write) -> io::Result<()> {
        for g in self.globals() {
            writeln!(output, "{}", g.directive.dump())?;
            writeln!(output, ".globl {}", g.label())?;
            writeln!(output, "{}:", g.label())?;
            for l in g.locals() {
                if let Some(label) = l.label() {
                    writeln!(output, "{}:", label)?;
                }
                for inst in l.insts() {
                    writeln!(output, "    {}", inst.dump())?;
                }
            }
        }
        Ok(())
    }
}
