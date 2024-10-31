use crate::backend::instruction::{AsmProgram, Inst};

pub struct AsmHelper {
    asm: AsmProgram,
    res: AsmProgram,
}

pub struct Cursor<'a> {
    helper: &'a mut AsmHelper,
    tot_len: usize,
    idx: usize,
    remove: bool,
}

impl AsmHelper {
    pub fn new(asm: AsmProgram) -> Self {
        Self {
            asm,
            res: AsmProgram::new(),
        }
    }

    pub fn new_cursor(&mut self) -> Cursor {
        let tot_len = self.asm.len();
        Cursor {
            helper: self,
            idx: 0,
            tot_len,
            remove: false,
        }
    }

    pub fn result(self) -> AsmProgram {
        self.res
    }
}

impl<'a> Cursor<'a> {
    pub fn next(&mut self) {
        if !self.remove {
            let inst = self.helper.asm[self.idx].clone();
            self.helper.res.push(inst);
        }
        self.remove = false;
        self.idx += 1;
    }

    pub fn end(&self) -> bool {
        self.idx >= self.tot_len
    }

    pub fn current(&self) -> &Inst {
        &self.helper.asm[self.idx]
    }

    pub fn remove_cur(&mut self) {
        self.remove = true;
    }

    pub fn insert(&mut self, inst: Inst) {
        self.helper.res.push(inst);
    }
}
