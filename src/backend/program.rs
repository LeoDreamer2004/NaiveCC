use super::instruction::{Inst, Label};
use crate::utils::namer::UniqueNameGenerator;
use std::io;

// pub type AsmProgram = Vec<Inst>;
pub struct AsmProgram {
    globals: Vec<AsmGlobal>,
}

#[derive(Debug, Clone)]
pub struct AsmGlobal {
    local_namer: UniqueNameGenerator,
    section: Section,
    label: Label,
    locals: Vec<AsmLocal>,
}

#[derive(Debug, Clone)]
pub struct AsmLocal {
    label: Option<Label>,
    insts: Vec<Inst>,
}


/// **Section**: .text or .data.
#[derive(Debug, Clone)]
pub enum Section {
    Text,
    Data,
}

impl Section {
    pub fn dump(&self) -> String {
        match self {
            Section::Text => String::from("\n.text"),
            Section::Data => String::from("\n.data"),
        }
    }
}

impl AsmGlobal {
    pub fn new(section: Section, label: Label) -> Self {
        AsmGlobal {
            local_namer: UniqueNameGenerator::default(),
            section,
            label,
            locals: Vec::new(),
        }
    }

    pub fn new_from(other: &AsmGlobal) -> Self {
        Self::new(other.section().clone(), other.label().clone())
    }

    pub fn section(&self) -> &Section {
        &self.section
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

    pub fn emit(&self, mut output: impl io::Write) -> io::Result<()> {
        for g in self.globals() {
            writeln!(output, "{}", g.section.dump())?;
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
