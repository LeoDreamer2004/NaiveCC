//! all the instructions in the RiscV assembly language.

use super::{registers::Register, Imm12, Imm32};
use std::fmt::{Debug, Formatter, Result as FmtResult};

pub type Label = String;

#[derive(Clone, Hash, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Inst {
    /// **Nop**: a no operation instruction.
    Nop,
    /// **Placeholder(u8)**: a placeholder for the instruction.
    Placeholder(u8),
    /// **Comment(comment)**: a comment.
    Comment(String),
    /// **Zero(size)**: reserve the memory with the size.
    Zero(usize),
    /// **Word(int)**: store the integer to the memory.
    Word(i32),
    /// **Beqz(rs, label)**: go to the label if rs is zero.
    Beqz(Register, Label),
    /// **Bnez(rs, label)**: go to the label if rs is not zero.
    Bnez(Register, Label),
    /// **J(label)**: jump to the label.
    J(Label),
    /// **Call(label)**: call the function.
    Call(Label),
    /// **Ret**: return from the function.
    Ret,
    /// **Lw(rd, rs, offset)**: load the memory at rs + offset to rd.
    Lw(Register, Register, Imm12),
    /// **Sw(rs2, rs1, offset)**: store the value of rs2 to rs1 + offset.
    Sw(Register, Register, Imm12),
    /// **Add(rd, rs1, rs2)**: add rs1 and rs2, and store the result to rd.
    Add(Register, Register, Register),
    /// **Addi(rd, rs, imm)**: add rs and imm, and store the result to rd.
    Addi(Register, Register, Imm12),
    /// **Sub(rd, rs1, rs2)**: subtract rs2 from rs1, and store the result to rd.
    Sub(Register, Register, Register),
    /// **Slt(rd, rs1, rs2)**: compare if rs1 is less than rs2, and store the result to rd.
    Slt(Register, Register, Register),
    /// **Sgt(rd, rs1, rs2)**: compare if rs1 is greater than rs2, and store the result to rd.
    Sgt(Register, Register, Register),
    /// **SeqZ(rd, rs)**: Judge if rs is zero, and store the result to rd.
    SeqZ(Register, Register),
    /// **SneZ(rd, rs)**: Judge if rs is not zero, and store the result to rd.
    SneZ(Register, Register),
    /// **Xor(rd, rs1, rs2)**: calculate the bitwise xor of rs1 and rs2, and store the result to rd.
    Xor(Register, Register, Register),
    /// **Xori(rd, rs, imm)**: calculate the bitwise xor of rs and imm, and store the result to rd.
    Xori(Register, Register, Imm12),
    /// **Or(rd, rs1, rs2)**: calculate the bitwise or of rs1 and rs2, and store the result to rd.
    Or(Register, Register, Register),
    /// **Ori(rd, rs, imm)**: calculate the bitwise or of rs and imm, and store the result to rd.
    Ori(Register, Register, Imm12),
    /// **And(rd, rs1, rs2)**: calculate the bitwise and of rs1 and rs2, and store the result to rd.
    And(Register, Register, Register),
    /// **Andi(rd, rs, imm)**: calculate the bitwise and of rs and imm, and store the result to rd.
    Andi(Register, Register, Imm12),
    /// **Sll(rd, rs1, rs2)**: shift left logical rs1 by rs2 bits, and store the result to rd.
    Sll(Register, Register, Register),
    /// **Slli(rd, rs, imm)**: shift left logical rs by imm bits, and store the result to rd.
    Slli(Register, Register, Imm12),
    /// **Srl(rd, rs1, rs2)**: shift right logical rs1 by rs2 bits, and store the result to rd.
    Srl(Register, Register, Register),
    /// **Srli(rd, rs, imm)**: shift right logical rs by imm bits, and store the result to rd.
    Srli(Register, Register, Imm12),
    /// **Sra(rd, rs1, rs2)**: shift right arithmetic rs1 by rs2 bits, and store the result to rd.
    Sra(Register, Register, Register),
    /// **Srai(rd, rs, imm)**: shift right arithmetic rs by imm bits, and store the result to rd.
    Srai(Register, Register, Imm12),
    /// **Mul(rd, rs1, rs2)**: multiply rs1 by rs2, and store the result to rd.
    Mul(Register, Register, Register),
    /// **Div(rd, rs1, rs2)**: divide rs1 by rs2, and store the result to rd.
    Div(Register, Register, Register),
    /// **Rem(rd, rs1, rs2)**: calculate the remainder of rs1 divided by rs2, and store the result to rd.
    Rem(Register, Register, Register),
    /// **Li(rd, imm)**: load the immediate value to rd.
    Li(Register, Imm32),
    /// **La(rd, label)**: load the address of label to rd.
    La(Register, Label),
    /// **Mv(rd, rs)**: move the value of rs to rd.
    Mv(Register, Register),
}

impl Inst {
    /// Write the instruction to the string.
    pub fn dump(&self) -> String {
        match self {
            Inst::Nop => String::new(),
            Inst::Placeholder(_) => String::new(),
            Inst::Comment(comment) => format!("# {}", comment),
            Inst::Zero(size) => format!(".zero {}", size),
            Inst::Word(int) => format!(".word {}", int),
            Inst::Beqz(rs, label) => format!("beqz {}, {}", rs, label),
            Inst::Bnez(rs, label) => format!("bnez {}, {}", rs, label),
            Inst::J(label) => format!("j {}", label),
            Inst::Call(label) => format!("call {}", label),
            Inst::Ret => format!("ret"),
            Inst::Lw(rd, rs, offset) => format!("lw {}, {}({})", rd, offset, rs),
            Inst::Sw(rs2, rs1, offset) => format!("sw {}, {}({})", rs2, offset, rs1),
            Inst::Add(rd, rs1, rs2) => format!("add {}, {}, {}", rd, rs1, rs2),
            Inst::Addi(rd, rs, imm) => format!("addi {}, {}, {}", rd, rs, imm),
            Inst::Sub(rd, rs1, rs2) => format!("sub {}, {}, {}", rd, rs1, rs2),
            Inst::Slt(rd, rs1, rs2) => format!("slt {}, {}, {}", rd, rs1, rs2),
            Inst::Sgt(rd, rs1, rs2) => format!("sgt {}, {}, {}", rd, rs1, rs2),
            Inst::SeqZ(rd, rs) => format!("seqz {}, {}", rd, rs),
            Inst::SneZ(rd, rs) => format!("snez {}, {}", rd, rs),
            Inst::Xor(rd, rs1, rs2) => format!("xor {}, {}, {}", rd, rs1, rs2),
            Inst::Xori(rd, rs, imm) => format!("xori {}, {}, {}", rd, rs, imm),
            Inst::Or(rd, rs1, rs2) => format!("or {}, {}, {}", rd, rs1, rs2),
            Inst::Ori(rd, rs, imm) => format!("ori {}, {}, {}", rd, rs, imm),
            Inst::And(rd, rs1, rs2) => format!("and {}, {}, {}", rd, rs1, rs2),
            Inst::Andi(rd, rs, imm) => format!("andi {}, {}, {}", rd, rs, imm),
            Inst::Sll(rd, rs1, rs2) => format!("sll {}, {}, {}", rd, rs1, rs2),
            Inst::Slli(rd, rs, imm) => format!("slli {}, {}, {}", rd, rs, imm),
            Inst::Srl(rd, rs1, rs2) => format!("srl {}, {}, {}", rd, rs1, rs2),
            Inst::Srli(rd, rs, imm) => format!("srli {}, {}, {}", rd, rs, imm),
            Inst::Sra(rd, rs1, rs2) => format!("sra {}, {}, {}", rd, rs1, rs2),
            Inst::Srai(rd, rs, imm) => format!("srai {}, {}, {}", rd, rs, imm),
            Inst::Mul(rd, rs1, rs2) => format!("mul {}, {}, {}", rd, rs1, rs2),
            Inst::Div(rd, rs1, rs2) => format!("div {}, {}, {}", rd, rs1, rs2),
            Inst::Rem(rd, rs1, rs2) => format!("rem {}, {}, {}", rd, rs1, rs2),
            Inst::Li(rd, imm) => format!("li {}, {}", rd, imm),
            Inst::La(rd, label) => format!("la {}, {}", rd, label),
            Inst::Mv(rd, rs) => format!("mv {}, {}", rd, rs),
        }
    }

    /// Get the registers that the instruction uses.
    pub fn regs_mut(&mut self) -> Vec<&mut Register> {
        match self {
            Inst::Nop => vec![],
            Inst::Placeholder(_) => vec![],
            Inst::Comment(_) => vec![],
            Inst::Zero(_) => vec![],
            Inst::Word(_) => vec![],
            Inst::Beqz(rs, _) => vec![rs],
            Inst::Bnez(rs, _) => vec![rs],
            Inst::J(_) => vec![],
            Inst::Call(_) => vec![],
            Inst::Ret => vec![],
            Inst::Lw(rd, rs, _) => vec![rd, rs],
            Inst::Sw(rs2, rs1, _) => vec![rs2, rs1],
            Inst::Add(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Addi(rd, rs, _) => vec![rd, rs],
            Inst::Sub(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Slt(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Sgt(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::SeqZ(rd, rs) => vec![rd, rs],
            Inst::SneZ(rd, rs) => vec![rd, rs],
            Inst::Xor(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Xori(rd, rs, _) => vec![rd, rs],
            Inst::Or(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Ori(rd, rs, _) => vec![rd, rs],
            Inst::And(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Andi(rd, rs, _) => vec![rd, rs],
            Inst::Sll(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Slli(rd, rs, _) => vec![rd, rs],
            Inst::Srl(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Srli(rd, rs, _) => vec![rd, rs],
            Inst::Sra(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Srai(rd, rs, _) => vec![rd, rs],
            Inst::Mul(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Div(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Rem(rd, rs1, rs2) => vec![rd, rs1, rs2],
            Inst::Li(rd, _) => vec![rd],
            Inst::La(rd, _) => vec![rd],
            Inst::Mv(rd, rs) => vec![rd, rs],
        }
    }

    /// Get the registers that the instruction uses.
    pub fn regs(&self) -> Vec<&Register> {
        let mut regs = self.src_regs();
        if let Some(rd) = self.dest_reg() {
            regs.push(rd);
        }
        regs
    }

    /// Get the destination register of the instruction.
    pub fn dest_reg(&self) -> Option<&Register> {
        match self {
            Inst::Lw(rd, _, _) => Some(rd),
            Inst::Add(rd, _, _) => Some(rd),
            Inst::Addi(rd, _, _) => Some(rd),
            Inst::Sub(rd, _, _) => Some(rd),
            Inst::Slt(rd, _, _) => Some(rd),
            Inst::Sgt(rd, _, _) => Some(rd),
            Inst::SeqZ(rd, _) => Some(rd),
            Inst::SneZ(rd, _) => Some(rd),
            Inst::Xor(rd, _, _) => Some(rd),
            Inst::Xori(rd, _, _) => Some(rd),
            Inst::Or(rd, _, _) => Some(rd),
            Inst::Ori(rd, _, _) => Some(rd),
            Inst::And(rd, _, _) => Some(rd),
            Inst::Andi(rd, _, _) => Some(rd),
            Inst::Sll(rd, _, _) => Some(rd),
            Inst::Slli(rd, _, _) => Some(rd),
            Inst::Srl(rd, _, _) => Some(rd),
            Inst::Srli(rd, _, _) => Some(rd),
            Inst::Sra(rd, _, _) => Some(rd),
            Inst::Srai(rd, _, _) => Some(rd),
            Inst::Mul(rd, _, _) => Some(rd),
            Inst::Div(rd, _, _) => Some(rd),
            Inst::Rem(rd, _, _) => Some(rd),
            Inst::Li(rd, _) => Some(rd),
            Inst::La(rd, _) => Some(rd),
            Inst::Mv(rd, _) => Some(rd),
            _ => None,
        }
    }

    /// Get the source registers of the instruction.
    pub fn src_regs(&self) -> Vec<&Register> {
        match self {
            Inst::Lw(_, rs, _) => vec![rs],
            Inst::Sw(rs2, rs1, _) => vec![rs1, rs2],
            Inst::Add(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Addi(_, rs, _) => vec![rs],
            Inst::Beqz(rs, _) => vec![rs],
            Inst::Bnez(rs, _) => vec![rs],
            Inst::Sub(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Slt(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Sgt(_, rs1, rs2) => vec![rs1, rs2],
            Inst::SeqZ(_, rs) => vec![rs],
            Inst::SneZ(_, rs) => vec![rs],
            Inst::Xor(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Xori(_, rs, _) => vec![rs],
            Inst::Or(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Ori(_, rs, _) => vec![rs],
            Inst::And(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Andi(_, rs, _) => vec![rs],
            Inst::Sll(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Slli(_, rs, _) => vec![rs],
            Inst::Srl(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Srli(_, rs, _) => vec![rs],
            Inst::Sra(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Srai(_, rs, _) => vec![rs],
            Inst::Mul(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Div(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Rem(_, rs1, rs2) => vec![rs1, rs2],
            Inst::Mv(_, rs) => vec![rs],
            _ => vec![],
        }
    }
}

impl Debug for Inst {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.dump())
    }
}
