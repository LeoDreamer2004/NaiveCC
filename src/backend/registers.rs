//! Registers in the RISC-V backend

use std::fmt::{self, Debug};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    RiscV(RiscVRegister),
    Fake(FakeRegister),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FakeRegister(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RiscVRegister(pub &'static str);

impl Default for Register {
    fn default() -> Self {
        ZERO
    }
}

impl Debug for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::RiscV(reg) => write!(f, "{}", reg.0),
            Register::Fake(reg) => write!(f, "reg_{}", reg.0),
        }
    }
}
/// Zero register
pub const ZERO: Register = Register::RiscV(RiscVRegister("zero"));

/// Return values
pub const A0: Register = Register::RiscV(RiscVRegister("a0"));
pub const A1: Register = Register::RiscV(RiscVRegister("a1"));

/// Argument registers
pub const A2: Register = Register::RiscV(RiscVRegister("a2"));
pub const A3: Register = Register::RiscV(RiscVRegister("a3"));
pub const A4: Register = Register::RiscV(RiscVRegister("a4"));
pub const A5: Register = Register::RiscV(RiscVRegister("a5"));
pub const A6: Register = Register::RiscV(RiscVRegister("a6"));
pub const A7: Register = Register::RiscV(RiscVRegister("a7"));

/// Saved registers
pub const S1: Register = Register::RiscV(RiscVRegister("s1"));
pub const S2: Register = Register::RiscV(RiscVRegister("s2"));
pub const S3: Register = Register::RiscV(RiscVRegister("s3"));
pub const S4: Register = Register::RiscV(RiscVRegister("s4"));
pub const S5: Register = Register::RiscV(RiscVRegister("s5"));
pub const S6: Register = Register::RiscV(RiscVRegister("s6"));
pub const S7: Register = Register::RiscV(RiscVRegister("s7"));
pub const S8: Register = Register::RiscV(RiscVRegister("s8"));
pub const S9: Register = Register::RiscV(RiscVRegister("s9"));
pub const S10: Register = Register::RiscV(RiscVRegister("s10"));
pub const S11: Register = Register::RiscV(RiscVRegister("s11"));

/// Temporary registers
pub const T0: Register = Register::RiscV(RiscVRegister("t0"));
pub const T1: Register = Register::RiscV(RiscVRegister("t1"));
pub const T2: Register = Register::RiscV(RiscVRegister("t2"));
pub const T3: Register = Register::RiscV(RiscVRegister("t3"));
pub const T4: Register = Register::RiscV(RiscVRegister("t4"));
pub const T5: Register = Register::RiscV(RiscVRegister("t5"));
pub const T6: Register = Register::RiscV(RiscVRegister("t6"));

/// Stack pointer
pub const SP: Register = Register::RiscV(RiscVRegister("sp"));

/// Return address
pub const RA: Register = Register::RiscV(RiscVRegister("ra"));

/// Frame pointer
pub const FP: Register = Register::RiscV(RiscVRegister("fp"));

pub enum RegisterType {
    Arg,
    Temp,
}

pub const FREE_REG: Register = T0;

impl RegisterType {
    const ARGU_REGISTERS: [Register; 8] = [A0, A1, A2, A3, A4, A5, A6, A7];
    const TEMP_REGISTERS: [Register; 17] = [
        T1, T2, T3, T4, T5, T6, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11,
    ];

    pub fn all(&self) -> &[Register] {
        match self {
            RegisterType::Arg => &Self::ARGU_REGISTERS,
            RegisterType::Temp => &Self::TEMP_REGISTERS,
        }
    }
}
