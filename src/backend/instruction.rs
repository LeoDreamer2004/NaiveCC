use super::register::Register;

pub type AsmProgram = Vec<Inst>;
pub type Label = String;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Inst {
    Nop,
    Placeholder(u8),
    Comment(String),
    Label(Label),
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
    Slli(Slli),
    Srl(Srl),
    Srli(Srli),
    Sra(Sra),
    Srai(Srai),
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
            Inst::Nop => String::new(),
            Inst::Placeholder(_) => String::new(),
            Inst::Comment(comment) => format!("# {}", comment),
            Inst::Label(label) => format!("{}:", label),
            Inst::Directive(directive) => format!("{}", directive.dump()),
            Inst::Beqz(beqz) => format!("beqz {}, {}", beqz.0, beqz.1),
            Inst::Bnez(bnez) => format!("bnez {}, {}", bnez.0, bnez.1),
            Inst::J(j) => format!("j {}", j.0),
            Inst::Call(call) => format!("call {}", call.0),
            Inst::Ret(_) => format!("ret"),
            Inst::Lw(lw) => format!("lw {}, {}({})", lw.0, lw.2, lw.1),
            Inst::Sw(sw) => format!("sw {}, {}({})", sw.0, sw.2, sw.1),
            Inst::Add(add) => format!("add {}, {}, {}", add.0, add.1, add.2),
            Inst::Addi(addi) => format!("addi {}, {}, {}", addi.0, addi.1, addi.2),
            Inst::Sub(sub) => format!("sub {}, {}, {}", sub.0, sub.1, sub.2),
            Inst::Slt(slt) => format!("slt {}, {}, {}", slt.0, slt.1, slt.2),
            Inst::Sgt(sgt) => format!("sgt {}, {}, {}", sgt.0, sgt.1, sgt.2),
            Inst::SeqZ(seq_z) => format!("seqz {}, {}", seq_z.0, seq_z.1),
            Inst::SneZ(sne_z) => format!("snez {}, {}", sne_z.0, sne_z.1),
            Inst::Xor(xor) => format!("xor {}, {}, {}", xor.0, xor.1, xor.2),
            Inst::Xori(xori) => format!("xori {}, {}, {}", xori.0, xori.1, xori.2),
            Inst::Or(or) => format!("or {}, {}, {}", or.0, or.1, or.2),
            Inst::Ori(ori) => format!("ori {}, {}, {}", ori.0, ori.1, ori.2),
            Inst::And(and) => format!("and {}, {}, {}", and.0, and.1, and.2),
            Inst::Andi(andi) => format!("andi {}, {}, {}", andi.0, andi.1, andi.2),
            Inst::Sll(sll) => format!("sll {}, {}, {}", sll.0, sll.1, sll.2),
            Inst::Slli(slli) => format!("slli {}, {}, {}", slli.0, slli.1, slli.2),
            Inst::Srl(srl) => format!("srl {}, {}, {}", srl.0, srl.1, srl.2),
            Inst::Srli(srli) => format!("srli {}, {}, {}", srli.0, srli.1, srli.2),
            Inst::Sra(sra) => format!("sra {}, {}, {}", sra.0, sra.1, sra.2),
            Inst::Srai(srai) => format!("srai {}, {}, {}", srai.0, srai.1, srai.2),
            Inst::Mul(mul) => format!("mul {}, {}, {}", mul.0, mul.1, mul.2),
            Inst::Div(div) => format!("div {}, {}, {}", div.0, div.1, div.2),
            Inst::Rem(rem) => format!("rem {}, {}, {}", rem.0, rem.1, rem.2),
            Inst::Li(li) => format!("li {}, {}", li.0, li.1),
            Inst::La(la) => format!("la {}, {}", la.0, la.1),
            Inst::Mv(mv) => format!("mv {}, {}", mv.0, mv.1),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Directive {
    Text,
    Data,
    Globl(String),
    Zero(usize),
    Word(Imm32),
}

impl Directive {
    pub fn dump(&self) -> String {
        match self {
            Directive::Text => String::from("\n.text"),
            Directive::Data => String::from("\n.data"),
            Directive::Globl(label) => format!(".globl {}", label),
            Directive::Zero(size) => format!(".zero {}", size),
            Directive::Word(int) => format!(".word {}", int),
        }
    }
}

pub type Imm32 = i32;
pub type Imm12 = i32;

/// **Beqz(rs, label)**: go to the label if rs is zero.
#[derive(Debug, Default, Clone)]
pub struct Beqz(pub Register, pub Label);

/// **Bnez(rs, label)**: go to the label if rs is not zero.
#[derive(Debug, Default, Clone)]
pub struct Bnez(pub Register, pub Label);

/// **J(label)**: jump to the label.
#[derive(Debug, Default, Clone)]
pub struct J(pub Label);

/// **Call(label)**: call the function.
#[derive(Debug, Default, Clone)]
pub struct Call(pub Label);

/// **Ret**: return from the function.
#[derive(Debug, Default, Clone)]
pub struct Ret;

/// **Lw(rd, rs, offset)**: load the memory at rs + offset to rd.
#[derive(Debug, Default, Clone)]
pub struct Lw(pub Register, pub Register, pub Imm12);

/// **Sw(rs2, rs1, offset)**: store the value of rs2 to rs1 + offset.
#[derive(Debug, Default, Clone)]
pub struct Sw(pub Register, pub Register, pub Imm12);

/// **Add(rd, rs1, rs2)**: add rs1 and rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Add(pub Register, pub Register, pub Register);

/// **Addi(rd, rs, imm)**: add rs and imm, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Addi(pub Register, pub Register, pub Imm12);

/// **Sub(rd, rs1, rs2)**: subtract rs2 from rs1, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Sub(pub Register, pub Register, pub Register);

/// **Slt(rd, rs1, rs2)**: compare if rs1 is less than rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Slt(pub Register, pub Register, pub Register);

/// **Sgt(rd, rs1, rs2)**: compare if rs1 is greater than rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Sgt(pub Register, pub Register, pub Register);

/// **SeqZ(rd, rs)**: Judge if rs is zero, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct SeqZ(pub Register, pub Register);

/// **SneZ(rd, rs)**: Judge if rs is not zero, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct SneZ(pub Register, pub Register);

/// **Xor(rd, rs1, rs2)**: calculate the bitwise xor of rs1 and rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Xor(pub Register, pub Register, pub Register);

/// **Xori(rd, rs, imm)**: calculate the bitwise xor of rs and imm, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Xori(pub Register, pub Register, pub Imm12);

/// **Or(rd, rs1, rs2)**: calculate the bitwise or of rs1 and rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Or(pub Register, pub Register, pub Register);

/// **Ori(rd, rs, imm)**: calculate the bitwise or of rs and imm, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Ori(pub Register, pub Register, pub Imm12);

/// **And(rd, rs1, rs2)**: calculate the bitwise and of rs1 and rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct And(pub Register, pub Register, pub Register);

/// **Andi(rd, rs, imm)**: calculate the bitwise and of rs and imm, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Andi(pub Register, pub Register, pub Imm12);

/// **Sll(rd, rs1, rs2)**: shift left logical rs1 by rs2 bits, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Sll(pub Register, pub Register, pub Register);

/// **Slli(rd, rs, imm)**: shift left logical rs by imm bits, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Slli(pub Register, pub Register, pub Imm12);

/// **Srl(rd, rs1, rs2)**: shift right logical rs1 by rs2 bits, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Srl(pub Register, pub Register, pub Register);

/// **Srli(rd, rs, imm)**: shift right logical rs by imm bits, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Srli(pub Register, pub Register, pub Imm12);

/// **Sra(rd, rs1, rs2)**: shift right arithmetic rs1 by rs2 bits, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Sra(pub Register, pub Register, pub Register);

/// **Srai(rd, rs, imm)**: shift right arithmetic rs by imm bits, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Srai(pub Register, pub Register, pub Imm12);

/// **Mul(rd, rs1, rs2)**: multiply rs1 by rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Mul(pub Register, pub Register, pub Register);

/// **Div(rd, rs1, rs2)**: divide rs1 by rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Div(pub Register, pub Register, pub Register);

/// **Rem(rd, rs1, rs2)**: calculate the remainder of rs1 divided by rs2, and store the result to rd.
#[derive(Debug, Default, Clone)]
pub struct Rem(pub Register, pub Register, pub Register);

/// **Li(rd, imm)**: load the immediate value to rd.
#[derive(Debug, Default, Clone)]
pub struct Li(pub Register, pub Imm32);

/// **La(rd, label)**: load the address of label to rd.
#[derive(Debug, Default, Clone)]
pub struct La(pub Register, pub Label);

/// **Mv(rd, rs)**: move the value of rs to rd.
#[derive(Debug, Default, Clone)]
pub struct Mv(pub Register, pub Register);
