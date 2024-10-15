use super::AsmError;
use koopa::ir::{entities::ValueData, Value};
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Register {
    pub name: &'static str,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
/// Zero register
pub const ZERO: Register = Register { name: "zero" };

/// Return values
pub const A0: Register = Register { name: "a0" };
pub const A1: Register = Register { name: "a1" };

/// Argument registers
pub const A2: Register = Register { name: "a2" };
pub const A3: Register = Register { name: "a3" };
pub const A4: Register = Register { name: "a4" };
pub const A5: Register = Register { name: "a5" };
pub const A6: Register = Register { name: "a6" };
pub const A7: Register = Register { name: "a7" };

/// Saved registers
pub const S1: Register = Register { name: "s1" };
pub const S2: Register = Register { name: "s2" };
pub const S3: Register = Register { name: "s3" };
pub const S4: Register = Register { name: "s4" };
pub const S5: Register = Register { name: "s5" };
pub const S6: Register = Register { name: "s6" };
pub const S7: Register = Register { name: "s7" };
pub const S8: Register = Register { name: "s8" };
pub const S9: Register = Register { name: "s9" };
pub const S10: Register = Register { name: "s10" };
pub const S11: Register = Register { name: "s11" };

/// Temporary registers
pub const T0: Register = Register { name: "t0" };
pub const T1: Register = Register { name: "t1" };
pub const T2: Register = Register { name: "t2" };
pub const T3: Register = Register { name: "t3" };
pub const T4: Register = Register { name: "t4" };
pub const T5: Register = Register { name: "t5" };
pub const T6: Register = Register { name: "t6" };

/// Stack pointer
pub const SP: Register = Register { name: "sp" };

/// Frame pointer
pub const FP: Register = Register { name: "fp" };

/// Return address
pub const RA: Register = Register { name: "ra" };

/// Global pointer
pub const GP: Register = Register { name: "gp" };

const ARGU_REGISTERS: [Register; 8] = [A0, A1, A2, A3, A4, A5, A6, A7];
const SAVED_REGISTERS: [Register; 11] = [S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11];
const TEMP_REGISTERS: [Register; 7] = [T0, T1, T2, T3, T4, T5, T6];

#[derive(Debug, Default)]
pub struct RegisterDispatcher {
    map: HashMap<*const ValueData, &'static Register>,
}

impl RegisterDispatcher {
    pub fn assign(&mut self, data: &ValueData, register: &'static Register) {
        let ptr: *const ValueData = data;
        self.map.insert(ptr, register);
    }

    pub fn alloc(&mut self) -> &'static Register {
        // for reg in ARGU_REGISTERS.iter() {
        //     if self.map.values().find(|&&r| r == reg).is_none() {
        //         return reg;
        //     }
        // };
        for reg in TEMP_REGISTERS.iter() {
            if self.map.values().find(|&&r| r == reg).is_none() {
                return reg;
            }
        }
        todo!()
    }

    pub fn get(&self, data: &ValueData) -> Option<&'static Register> {
        let ptr: *const ValueData = data;
        self.map.get(&ptr).copied()
    }

    pub fn get_or_error(&self, data: &ValueData) -> Result<&'static Register, AsmError> {
        self.get(data)
            .ok_or(AsmError::RegisterNotAssigned(data.name().clone()))
    }

    pub fn release(&mut self, data: &ValueData) {
        let ptr: *const ValueData = data;
        self.map.remove(&ptr);
    }
}
