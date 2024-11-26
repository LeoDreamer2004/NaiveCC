use super::super::instruction::*;
use super::super::is_imm12;
use super::super::program::AsmLocal;
use super::{AsmHelper, LocalOptimizer};

#[derive(Default)]
pub struct AlgorithmOptimizer;

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

impl LocalOptimizer for AlgorithmOptimizer {
    fn run(&mut self, asm: &AsmLocal) -> AsmLocal {
        let mut helper = AsmHelper::new(asm);
        let mut csr = helper.new_cursor();

        while !csr.end() {
            if csr.peek(1).is_none() {
                csr.go_to_end();
                break;
            }
            let next = csr.peek(1).unwrap();
            match (csr.current().clone(), next.clone()) {
                (Inst::Li(r, imm), Inst::Add(rd, rs1, rs2)) => {
                    csr.next();
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    csr.insert(Inst::Addi(rd, another, imm));
                }
                (Inst::Li(r, imm), Inst::Sub(rd, rs1, rs2)) => {
                    csr.next();
                    let another = check_reg!(r, -imm, rs1, rs2);
                    csr.remove_cur();
                    csr.insert(Inst::Addi(rd, another, -imm));
                }
                (Inst::Li(r, imm), Inst::Mul(rd, rs1, rs2)) => {
                    // maybe use bits to check if imm is a power of 2
                    if imm.count_ones() == 1 {
                        csr.next();
                        let another = check_reg!(r, imm, rs1, rs2);
                        csr.remove_cur();
                        csr.insert(Inst::Slli(rd, another, imm.trailing_zeros() as i32));
                    }
                }
                (Inst::Li(r, imm), Inst::Div(rd, rs1, rs2)) => {
                    if imm.count_ones() == 1 {
                        csr.next();
                        if !is_imm12(imm) || r != rs2 {
                            continue;
                        }
                        csr.remove_cur();
                        csr.insert(Inst::Srai(rd, rs1, imm.trailing_zeros() as i32));
                    }
                }

                (Inst::Li(r, imm), Inst::Rem(rd, rs1, rs2)) => {
                    if imm.count_ones() == 1 {
                        csr.next();
                        if !is_imm12(imm) || r != rs2 {
                            continue;
                        }
                        csr.remove_cur();
                        csr.insert(Inst::Andi(rd, rs1, imm - 1));
                    }
                }
                (Inst::Li(r, imm), Inst::Or(rd, rs1, rs2)) => {
                    csr.next();
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    csr.insert(Inst::Ori(rd, another, imm));
                }
                (Inst::Li(r, imm), Inst::And(rd, rs1, rs2)) => {
                    csr.next();
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    csr.insert(Inst::Andi(rd, another, imm));
                }
                (Inst::Li(r, imm), Inst::Xor(rd, rs1, rs2)) => {
                    csr.next();
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    csr.insert(Inst::Xori(rd, another, imm));
                }
                (Inst::Li(r, imm), Inst::Sll(rd, rs1, rs2)) => {
                    csr.next();
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    csr.insert(Inst::Slli(rd, another, imm));
                }
                (Inst::Li(r, imm), Inst::Srl(rd, rs1, rs2)) => {
                    csr.next();
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    csr.insert(Inst::Srli(rd, another, imm));
                }
                (Inst::Li(r, imm), Inst::Sra(rd, rs1, rs2)) => {
                    csr.next();
                    let another = check_reg!(r, imm, rs1, rs2);
                    csr.remove_cur();
                    csr.insert(Inst::Srai(rd, another, imm));
                }
                _ => {}
            };
            csr.next();
        }

        let asm = helper.result();
        let mut helper = AsmHelper::new(&asm);
        let mut csr = helper.new_cursor();
        while !csr.end() {
            match csr.current() {
                Inst::Addi(rs1, rs2, imm) => {
                    if *imm == 0 && rs1 == rs2 {
                        csr.remove_cur();
                    }
                }
                Inst::Ori(rs1, rs2, imm) => {
                    if *imm == 0 && rs1 == rs2 {
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
