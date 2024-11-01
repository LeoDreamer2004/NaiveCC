use std::collections::HashSet;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RiscVRegister {
    pub name: &'static str,
}

pub type Register = &'static RiscVRegister;

impl Default for Register {
    fn default() -> Self {
        ZERO
    }
}

impl fmt::Display for RiscVRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
/// Zero register
pub const ZERO: Register = &RiscVRegister { name: "zero" };

/// Return values
pub const A0: Register = &RiscVRegister { name: "a0" };
pub const A1: Register = &RiscVRegister { name: "a1" };

/// Argument registers
pub const A2: Register = &RiscVRegister { name: "a2" };
pub const A3: Register = &RiscVRegister { name: "a3" };
pub const A4: Register = &RiscVRegister { name: "a4" };
pub const A5: Register = &RiscVRegister { name: "a5" };
pub const A6: Register = &RiscVRegister { name: "a6" };
pub const A7: Register = &RiscVRegister { name: "a7" };

/// Saved registers
pub const S1: Register = &RiscVRegister { name: "s1" };
pub const S2: Register = &RiscVRegister { name: "s2" };
pub const S3: Register = &RiscVRegister { name: "s3" };
pub const S4: Register = &RiscVRegister { name: "s4" };
pub const S5: Register = &RiscVRegister { name: "s5" };
pub const S6: Register = &RiscVRegister { name: "s6" };
pub const S7: Register = &RiscVRegister { name: "s7" };
pub const S8: Register = &RiscVRegister { name: "s8" };
pub const S9: Register = &RiscVRegister { name: "s9" };
pub const S10: Register = &RiscVRegister { name: "s10" };
pub const S11: Register = &RiscVRegister { name: "s11" };

/// Temporary registers
pub const T0: Register = &RiscVRegister { name: "t0" };
pub const T1: Register = &RiscVRegister { name: "t1" };
pub const T2: Register = &RiscVRegister { name: "t2" };
pub const T3: Register = &RiscVRegister { name: "t3" };
pub const T4: Register = &RiscVRegister { name: "t4" };
pub const T5: Register = &RiscVRegister { name: "t5" };
pub const T6: Register = &RiscVRegister { name: "t6" };

/// Stack pointer
pub const SP: Register = &RiscVRegister { name: "sp" };

/// Return address
pub const RA: Register = &RiscVRegister { name: "ra" };

/// Frame pointer
pub const S0: Register = &RiscVRegister { name: "s0" };

pub enum RegisterType {
    Saved, // callee-saved
    Temp,  // caller-saved
}

pub const FREE_REG: Register = T0;
pub const ANY_REG: Register = &RiscVRegister { name: "any" };

impl RegisterType {
    pub const ARGU_REGISTERS: [Register; 8] = [A0, A1, A2, A3, A4, A5, A6, A7];
    pub const SAVED_REGISTERS: [Register; 11] = [S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11];
    pub const TEMP_REGISTERS: [Register; 6] = [T1, T2, T3, T4, T5, T6];

    pub fn all(&self) -> Vec<Register> {
        match self {
            RegisterType::Saved => Self::SAVED_REGISTERS.to_vec(),
            RegisterType::Temp => Self::TEMP_REGISTERS.to_vec(),
        }
    }
}

#[derive(Debug, Default)]
pub struct RegisterDispatcher {
    used: HashSet<Register>,
}

impl RegisterDispatcher {
    /// Ask for a register for the data.
    pub fn ask(&self, r_type: RegisterType) -> Option<Register> {
        for reg in r_type.all() {
            if self.askfor(reg) {
                return Some(reg);
            }
        }
        None
    }

    /// Ask if the register is available.
    pub fn askfor(&self, register: Register) -> bool {
        !self.used.contains(register)
    }

    /// Occupy the register.
    pub fn occupy(&mut self, register: Register) {
        self.used.insert(register);
    }

    /// Release the register.
    pub fn release(&mut self, register: Register) {
        self.used.remove(register);
    }

    /// Ask, occupy and return the register.
    pub fn dispatch(&mut self, r_type: RegisterType) -> Option<Register> {
        let reg = self.ask(r_type);
        if let Some(r) = reg {
            self.occupy(r);
        }
        reg
    }
}
