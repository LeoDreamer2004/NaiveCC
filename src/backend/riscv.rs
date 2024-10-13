use std::fmt;


pub struct Register {
    pub name: &'static str,
}

pub mod registers {
    use super::Register;
    /// Zero register
    pub const ZERO : Register = Register { name: "zero" };

    /// Return values
    pub const A0 : Register = Register { name: "a0" };
    pub const A1 : Register = Register { name: "a1" };
    pub const A2 : Register = Register { name: "a2" };
    pub const A3 : Register = Register { name: "a3" };
    pub const A4 : Register = Register { name: "a4" };
    pub const A5 : Register = Register { name: "a5" };
    pub const A6 : Register = Register { name: "a6" };
    pub const A7 : Register = Register { name: "a7" };

    /// Saved registers
    pub const S1 : Register = Register { name: "s1" };
    pub const S2 : Register = Register { name: "s2" };
    pub const S3 : Register = Register { name: "s3" };
    pub const S4 : Register = Register { name: "s4" };
    pub const S5 : Register = Register { name: "s5" };
    pub const S6 : Register = Register { name: "s6" };
    pub const S7 : Register = Register { name: "s7" };
    pub const S8 : Register = Register { name: "s8" };
    pub const S9 : Register = Register { name: "s9" };
    pub const S10 : Register = Register { name: "s10" };
    pub const S11 : Register = Register { name: "s11" };

    /// Temporary registers
    pub const T0 : Register = Register { name: "t0" };
    pub const T1 : Register = Register { name: "t1" };
    pub const T2 : Register = Register { name: "t2" };
    pub const T3 : Register = Register { name: "t3" };
    pub const T4 : Register = Register { name: "t4" };
    pub const T5 : Register = Register { name: "t5" };
    pub const T6 : Register = Register { name: "t6" };

    /// Stack pointer
    pub const SP : Register = Register { name: "sp" };

    /// Frame pointer
    pub const FP : Register = Register { name: "fp" };

    /// Return address
    pub const RA : Register = Register { name: "ra" };

    /// Global pointer
    pub const GP : Register = Register { name: "gp" };
}

pub trait Instruction {
    fn dump(&self) -> String;
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub struct Mv {
    pub rd: Register,
    pub rs: Register,
}

impl Instruction for Mv {
    fn dump(&self) -> String {
        format!("mv {}, {}\n", self.rd, self.rs)
    }
}

pub struct Ret {}

impl Instruction for Ret {
    fn dump(&self) -> String {
        "ret\n".to_string()
    }
}
