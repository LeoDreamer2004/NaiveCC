use super::register::ConstRegister;

pub type AsmProgram = Vec<Inst>;

#[derive(Debug)]
pub enum Inst {
    Placeholder,
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
            Inst::Placeholder => String::new(),
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

#[derive(Debug)]
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
#[derive(Debug, Default)]
pub struct Beqz {
    pub rs: ConstRegister,
    pub label: String,
}

/// go to the label if rs is not zero.
#[derive(Debug, Default)]
pub struct Bnez {
    pub rs: ConstRegister,
    pub label: String,
}

/// go to the label.
#[derive(Debug, Default)]
pub struct J {
    pub label: String,
}

/// call the function.
#[derive(Debug, Default)]
pub struct Call {
    pub label: String,
}

/// return from the function.
#[derive(Debug, Default)]
pub struct Ret {}

/// load the memory at rs + offset to rd.
#[derive(Debug, Default)]
pub struct Lw {
    pub rd: ConstRegister,
    pub rs: ConstRegister,
    pub offset: i32,
}

/// store the memory at rs1 + offset from rs2.
#[derive(Debug, Default)]
pub struct Sw {
    pub rs2: ConstRegister,
    pub offset: i32,
    pub rs1: ConstRegister,
}

/// add rs1 and rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct Add {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// add rs and imm, and store the result to rd.
#[derive(Debug, Default)]
pub struct Addi {
    pub rd: ConstRegister,
    pub rs: ConstRegister,
    pub imm: i32,
}

/// subtract rs2 from rs1, and store the result to rd.
#[derive(Debug, Default)]
pub struct Sub {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// compare if rs1 is less than rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct Slt {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// compare if rs1 is greater than rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct Sgt {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// Judge if rs is zero, and store the result to rd.
#[derive(Debug, Default)]
pub struct SeqZ {
    pub rd: ConstRegister,
    pub rs: ConstRegister,
}

/// Judge if rs is not zero, and store the result to rd.
#[derive(Debug, Default)]
pub struct SneZ {
    pub rd: ConstRegister,
    pub rs: ConstRegister,
}

/// calculate the bitwise xor of rs1 and rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct Xor {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// calculate the bitwise xor of rs and imm, and store the result to rd.
#[derive(Debug, Default)]
pub struct Xori {
    pub rd: ConstRegister,
    pub rs: ConstRegister,
    pub imm: i32,
}

/// calculate the bitwise or of rs1 and rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct Or {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// calculate the bitwise or of rs and imm, and store the result to rd.
#[derive(Debug, Default)]
pub struct Ori {
    pub rd: ConstRegister,
    pub rs: ConstRegister,
    pub imm: i32,
}

/// calculate the bitwise and of rs1 and rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct And {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// calculate the bitwise and of rs and imm, and store the result to rd.
#[derive(Debug, Default)]
pub struct Andi {
    pub rd: ConstRegister,
    pub rs: ConstRegister,
    pub imm: i32,
}

/// shift left logical rs1 by rs2 bits, and store the result to rd.
#[derive(Debug, Default)]
pub struct Sll {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// shift right logical rs1 by rs2 bits, and store the result to rd.
#[derive(Debug, Default)]
pub struct Srl {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// shift right arithmetic rs1 by rs2 bits, and store the result to rd.
#[derive(Debug, Default)]
pub struct Sra {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// multiply rs1 and rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct Mul {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// divide rs1 by rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct Div {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// calculate the remainder of rs1 divided by rs2, and store the result to rd.
#[derive(Debug, Default)]
pub struct Rem {
    pub rd: ConstRegister,
    pub rs1: ConstRegister,
    pub rs2: ConstRegister,
}

/// load the immediate value to rd.
#[derive(Debug, Default)]
pub struct Li {
    pub rd: ConstRegister,
    pub imm: i32,
}

/// load the address of the label to rd.
#[derive(Debug, Default)]
pub struct La {
    pub rd: ConstRegister,
    pub label: String,
}

/// move the value of rs to rd.
#[derive(Debug, Default)]
pub struct Mv {
    pub rd: ConstRegister,
    pub rs: ConstRegister,
}
