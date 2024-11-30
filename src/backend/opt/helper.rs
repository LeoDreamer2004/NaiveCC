use super::super::instruction::Inst;
use super::super::program::AsmLocal;

pub struct OptHelper {
    insts: Vec<Inst>,
    res: AsmLocal,
}

pub struct Cursor<'a> {
    helper: &'a mut OptHelper,
    tot_len: usize,
    idx: usize,
    remove: bool,
}

impl OptHelper {
    pub fn new(asm: &AsmLocal) -> Self {
        let res = AsmLocal::new_from(asm);
        let insts = asm.insts().clone();
        Self { insts, res }
    }

    pub fn new_cursor(&mut self) -> Cursor {
        let tot_len = self.insts.len();
        Cursor {
            helper: self,
            idx: 0,
            tot_len,
            remove: false,
        }
    }

    pub fn result(self) -> AsmLocal {
        self.res
    }
}

impl<'a> Cursor<'a> {
    pub fn next(&mut self) {
        if !self.remove {
            let inst = self.helper.insts[self.idx].clone();
            self.helper.res.insts_mut().push(inst);
        }
        self.remove = false;
        self.idx += 1;
    }

    pub fn end(&self) -> bool {
        self.idx >= self.tot_len
    }

    pub fn go_to_end(&mut self) {
        while !self.end() {
            self.next();
        }
    }

    pub fn current(&self) -> &Inst {
        &self.helper.insts[self.idx]
    }

    pub fn peek(&self, bias: i32) -> Option<&Inst> {
        if (self.idx as i32 + bias) < 0 || (self.idx as i32 + bias) as usize >= self.tot_len {
            return None;
        }
        Some(&self.helper.insts[(self.idx as i32 + bias) as usize])
    }

    pub fn remove_cur(&mut self) {
        self.remove = true;
    }

    pub fn insert(&mut self, inst: Inst) {
        self.helper.res.insts_mut().push(inst);
    }
}
