use std::fmt::format;

use super::register::Register;

pub type AsmProgram = Vec<Inst>;

pub enum Inst {
    Label(String),
    Directive(Directive),
    Beqz(Beqz),
    Bnez(Bnez),
    J(J),
    Call(Call),
    Ret(Ret),
    Lw(Lw),
    Sw(Sw),
    Add(Add),
    Addi(Addi),
    Sub(Sub),
    Slt(Slt),
    Sgt(Sgt),
    SeqZ(SeqZ),
    SneZ(SneZ),
    Xor(Xor),
    Xori(Xori),
    Or(Or),
    Ori(Ori),
    And(And),
    Andi(Andi),
    Sll(Sll),
    Srl(Srl),
    Sra(Sra),
    Mul(Mul),
    Div(Div),
    Rem(Rem),
    Li(Li),
    La(La),
    Mv(Mv),
}

impl Inst {
    pub fn dump(&self) -> String {
        match self {
            Inst::Label(label) => format!("{}:", label),
            Inst::Directive(directive) => directive.dump(),
            Inst::Beqz(beqz) => format!("beqz\t{}, {}", beqz.rs, beqz.label),
            Inst::Bnez(bnez) => format!("bnez\t{}, {}", bnez.rs, bnez.label),
            Inst::J(j) => format!("j\t{}", j.label),
            Inst::Call(call) => format!("call\t{}", call.label),
            Inst::Ret(_) => String::from("ret"),
            Inst::Lw(lw) => format!("lw\t{}, {}({})", lw.rd, lw.offset, lw.rs),
            Inst::Sw(sw) => format!("sw\t{}, {}({})", sw.rs2, sw.offset, sw.rs1),
            Inst::Add(add) => format!("add\t{}, {}, {}", add.rd, add.rs1, add.rs2),
            Inst::Addi(addi) => format!("addi\t{}, {}, {}", addi.rd, addi.rs, addi.imm),
            Inst::Sub(sub) => format!("sub\t{}, {}, {}", sub.rd, sub.rs1, sub.rs2),
            Inst::Slt(slt) => format!("slt\t{}, {}, {}", slt.rd, slt.rs1, slt.rs2),
            Inst::Sgt(sgt) => format!("sgt\t{}, {}, {}", sgt.rd, sgt.rs1, sgt.rs2),
            Inst::SeqZ(seqz) => format!("seqz\t{}, {}", seqz.rd, seqz.rs),
            Inst::SneZ(snez) => format!("snez\t{}, {}", snez.rd, snez.rs),
            Inst::Xor(xor) => format!("xor\t{}, {}, {}", xor.rd, xor.rs1, xor.rs2),
            Inst::Xori(xori) => format!("xori\t{}, {}, {}", xori.rd, xori.rs, xori.imm),
            Inst::Or(or) => format!("or\t{}, {}, {}", or.rd, or.rs1, or.rs2),
            Inst::Ori(ori) => format!("ori\t{}, {}, {}", ori.rd, ori.rs, ori.imm),
            Inst::And(and) => format!("and\t{}, {}, {}", and.rd, and.rs1, and.rs2),
            Inst::Andi(andi) => format!("andi\t{}, {}, {}", andi.rd, andi.rs, andi.imm),
            Inst::Sll(sll) => format!("sll\t{}, {}, {}", sll.rd, sll.rs1, sll.rs2),
            Inst::Srl(srl) => format!("srl\t{}, {}, {}", srl.rd, srl.rs1, srl.rs2),
            Inst::Sra(sra) => format!("sra\t{}, {}, {}", sra.rd, sra.rs1, sra.rs2),
            Inst::Mul(mul) => format!("mul\t{}, {}, {}", mul.rd, mul.rs1, mul.rs2),
            Inst::Div(div) => format!("div\t{}, {}, {}", div.rd, div.rs1, div.rs2),
            Inst::Rem(rem) => format!("rem\t{}, {}, {}", rem.rd, rem.rs1, rem.rs2),
            Inst::Li(li) => format!("li\t{}, {}", li.rd, li.imm),
            Inst::La(la) => format!("la\t{}, {}", la.rd, la.label),
            Inst::Mv(mv) => format!("mv\t{}, {}", mv.rd, mv.rs),
        }
    }
}

pub enum Directive {
    Text,
    Data,
    Globl(String),
    Asciz(String),
    Word(i32),
}

impl Directive {
    pub fn dump(&self) -> String {
        match self {
            Directive::Text => String::from(".text"),
            Directive::Data => String::from(".data"),
            Directive::Globl(label) => format!(".globl {}", label),
            Directive::Asciz(string) => format!(".asciz \"{}\"", string),
            Directive::Word(int) => format!(".word {}", int),
        }
    }
}

/// go to the label if rs is zero.
pub struct Beqz {
    pub rs: &'static Register,
    pub label: String,
}

/// go to the label if rs is not zero.
pub struct Bnez {
    pub rs: &'static Register,
    pub label: String,
}

/// go to the label.
pub struct J {
    pub label: String,
}

/// call the function.
pub struct Call {
    pub label: String,
}

/// return from the function.
pub struct Ret {}

/// load the memory at rs + offset to rd.
pub struct Lw {
    pub rd: &'static Register,
    pub offset: i32,
    pub rs: &'static Register,
}

/// store the memory at rs1 + offset to rs2.
pub struct Sw {
    pub rs2: &'static Register,
    pub offset: i32,
    pub rs1: &'static Register,
}

/// add rs1 and rs2, and store the result to rd.
pub struct Add {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// add rs and imm, and store the result to rd.
pub struct Addi {
    pub rd: &'static Register,
    pub rs: &'static Register,
    pub imm: i32,
}

/// subtract rs2 from rs1, and store the result to rd.
pub struct Sub {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// compare if rs1 is less than rs2, and store the result to rd.
pub struct Slt {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// compare if rs1 is greater than rs2, and store the result to rd.
pub struct Sgt {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// Judge if rs is zero, and store the result to rd.
pub struct SeqZ {
    pub rd: &'static Register,
    pub rs: &'static Register,
}

/// Judge if rs is not zero, and store the result to rd.
pub struct SneZ {
    pub rd: &'static Register,
    pub rs: &'static Register,
}

/// calculate the bitwise xor of rs1 and rs2, and store the result to rd.
pub struct Xor {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// calculate the bitwise xor of rs and imm, and store the result to rd.
pub struct Xori {
    pub rd: &'static Register,
    pub rs: &'static Register,
    pub imm: i32,
}

/// calculate the bitwise or of rs1 and rs2, and store the result to rd.
pub struct Or {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// calculate the bitwise or of rs and imm, and store the result to rd.
pub struct Ori {
    pub rd: &'static Register,
    pub rs: &'static Register,
    pub imm: i32,
}

/// calculate the bitwise and of rs1 and rs2, and store the result to rd.
pub struct And {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// calculate the bitwise and of rs and imm, and store the result to rd.
pub struct Andi {
    pub rd: &'static Register,
    pub rs: &'static Register,
    pub imm: i32,
}

/// shift left logical rs1 by rs2 bits, and store the result to rd.
pub struct Sll {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// shift right logical rs1 by rs2 bits, and store the result to rd.
pub struct Srl {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// shift right arithmetic rs1 by rs2 bits, and store the result to rd.
pub struct Sra {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// multiply rs1 and rs2, and store the result to rd.
pub struct Mul {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// divide rs1 by rs2, and store the result to rd.
pub struct Div {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// calculate the remainder of rs1 divided by rs2, and store the result to rd.
pub struct Rem {
    pub rd: &'static Register,
    pub rs1: &'static Register,
    pub rs2: &'static Register,
}

/// load the immediate value to rd.
pub struct Li {
    pub rd: &'static Register,
    pub imm: i32,
}

/// load the address of the label to rd.
pub struct La {
    pub rd: &'static Register,
    pub label: String,
}

/// move the value of rs to rd.
pub struct Mv {
    pub rd: &'static Register,
    pub rs: &'static Register,
}
