use super::{AsmHelper, Optimizer};
use crate::backend::{instruction::*, is_imm12, register::Register};

pub struct AlgorithmOptimizer {}

impl AlgorithmOptimizer {
    pub fn new() -> Self {
        Self {}
    }
}

macro_rules! check_reg {
    ($r:expr, $imm:expr, $rs1:expr, $rs2:expr) => {{
        if !is_imm12($imm) {
            continue;
        }
        if $r == $rs1 {
            $rs2
        } else if $r == $rs2 {
            $rs1
        } else {
            continue;
        }
    }};
}
impl Optimizer for AlgorithmOptimizer {
    fn run(&mut self, asm: AsmProgram) -> AsmProgram {
        let mut helper = AsmHelper::new(asm);
        let mut csr = helper.new_cursor();

        let mut remove_next = false;
        while !csr.end() {
            if remove_next {
                csr.remove_cur();
                remove_next = false;
            }
            csr.next();
            if csr.peek(1).is_none() {
                break;
            }
            let next = csr.peek(1).unwrap();
            match (csr.current().clone(), next.clone()) {
                (Inst::Li(Li(r, imm)), Inst::Add(Add(rd, rs1, rs2))) => {
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    remove_next = true;
                    csr.insert(Inst::Addi(Addi(rd, another, imm)));
                }
                (Inst::Li(Li(r, imm)), Inst::Mul(Mul(rd, rs1, rs2))) => {
                    let another = check_reg!(r, imm, rs1, rs2);
                    // maybe use bits to check if imm is a power of 2
                    if imm.count_ones() == 1 {
                        csr.remove_cur();
                        remove_next = true;
                        csr.insert(Inst::Slli(Slli(rd, another, imm.trailing_zeros() as i32)));
                    }
                }
                (Inst::Li(Li(r, imm)), Inst::Or(Or(rd, rs1, rs2))) => {
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    remove_next = true;
                    csr.insert(Inst::Ori(Ori(rd, another, imm)));
                }
                (Inst::Li(Li(r, imm)), Inst::And(And(rd, rs1, rs2))) => {
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    remove_next = true;
                    csr.insert(Inst::Andi(Andi(rd, another, imm)));
                }
                (Inst::Li(Li(r, imm)), Inst::Xor(Xor(rd, rs1, rs2))) => {
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    remove_next = true;
                    csr.insert(Inst::Xori(Xori(rd, another, imm)));
                }
                _ => {}
            };
        }

        let asm = helper.result();
        let mut helper = AsmHelper::new(asm);
        let mut csr = helper.new_cursor();
        while !csr.end() {
            match csr.current() {
                Inst::Addi(Addi(_, _, imm)) => {
                    if *imm == 0 {
                        csr.remove_cur();
                    }
                }
                Inst::Ori(Ori(_, _, imm)) => {
                    if *imm == 0 {
                        csr.remove_cur();
                    }
                }
                _ => {}
            }
            csr.next();
        }

        helper.result()
    }
}
