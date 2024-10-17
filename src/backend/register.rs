use super::{instruction::*, AsmError};
use koopa::ir::entities::ValueData;
use std::collections::{HashSet, LinkedList};
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Register {
    pub name: &'static str,
}

pub type RiscVRegister = &'static Register;

impl Default for RiscVRegister {
    fn default() -> Self {
        &ZERO
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
/// Zero register
pub const ZERO: RiscVRegister = &Register { name: "zero" };

/// Return values
pub const A0: RiscVRegister = &Register { name: "a0" };
pub const A1: RiscVRegister = &Register { name: "a1" };

/// Argument registers
pub const A2: RiscVRegister = &Register { name: "a2" };
pub const A3: RiscVRegister = &Register { name: "a3" };
pub const A4: RiscVRegister = &Register { name: "a4" };
pub const A5: RiscVRegister = &Register { name: "a5" };
pub const A6: RiscVRegister = &Register { name: "a6" };
pub const A7: RiscVRegister = &Register { name: "a7" };

/// Saved registers
pub const S1: RiscVRegister = &Register { name: "s1" };
pub const S2: RiscVRegister = &Register { name: "s2" };
pub const S3: RiscVRegister = &Register { name: "s3" };
pub const S4: RiscVRegister = &Register { name: "s4" };
pub const S5: RiscVRegister = &Register { name: "s5" };
pub const S6: RiscVRegister = &Register { name: "s6" };
pub const S7: RiscVRegister = &Register { name: "s7" };
pub const S8: RiscVRegister = &Register { name: "s8" };
pub const S9: RiscVRegister = &Register { name: "s9" };
pub const S10: RiscVRegister = &Register { name: "s10" };
pub const S11: RiscVRegister = &Register { name: "s11" };

/// Temporary registers
pub const T0: RiscVRegister = &Register { name: "t0" };
pub const T1: RiscVRegister = &Register { name: "t1" };
pub const T2: RiscVRegister = &Register { name: "t2" };
pub const T3: RiscVRegister = &Register { name: "t3" };
pub const T4: RiscVRegister = &Register { name: "t4" };
pub const T5: RiscVRegister = &Register { name: "t5" };
pub const T6: RiscVRegister = &Register { name: "t6" };

/// Stack pointer
pub const SP: RiscVRegister = &Register { name: "sp" };

/// Frame pointer
pub const FP: RiscVRegister = &Register { name: "fp" };

/// Return address
pub const RA: RiscVRegister = &Register { name: "ra" };

/// Global pointer
pub const GP: RiscVRegister = &Register { name: "gp" };

pub enum RegisterType {
    Argument,
    Saved,
    Temp,
    Local,
}

impl RegisterType {
    const ARGU_REGISTERS: [RiscVRegister; 6] = [A2, A3, A4, A5, A6, A7];
    const SAVED_REGISTERS: [RiscVRegister; 11] = [S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11];
    const TEMP_REGISTERS: [RiscVRegister; 7] = [T0, T1, T2, T3, T4, T5, T6];
    const LOCAL_REGISTERS: [RiscVRegister; 13] =
        [A2, A3, A4, A5, A6, A7, T0, T1, T2, T3, T4, T5, T6];

    pub fn all(&self) -> Vec<RiscVRegister> {
        match self {
            RegisterType::Argument => Self::ARGU_REGISTERS.to_vec(),
            RegisterType::Saved => Self::SAVED_REGISTERS.to_vec(),
            RegisterType::Temp => Self::TEMP_REGISTERS.to_vec(),
            RegisterType::Local => Self::LOCAL_REGISTERS.to_vec(),
        }
    }
}

pub type Pointer = *const ValueData;

macro_rules! as_ptr {
    ($data:expr) => {
        $data as *const ValueData
    };
}

#[derive(Debug, Default)]
pub struct RegisterDispatcher {
    map: HashMap<Pointer, Address>,
    frame_sizes: LinkedList<usize>,
    used: HashSet<RiscVRegister>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Address {
    Register(RiscVRegister),
    Stack(Stack),
}

/// Stack address, which grows from high to low.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack {
    offset: i32, // the offset from the stack pointer
}

impl RegisterDispatcher {
    pub fn ask(&self, r_type: RegisterType) -> RiscVRegister {
        for reg in r_type.all() {
            if self.askfor(reg) {
                return reg;
            }
        }
        todo!()
    }

    pub fn askfor(&self, register: RiscVRegister) -> bool {
        !self.used.contains(register)
    }

    pub fn occupy(&mut self, register: RiscVRegister) {
        self.used.insert(register);
    }

    pub fn release(&mut self, register: RiscVRegister) {
        self.used.remove(register);
    }

    pub fn dispatch(&mut self, r_type: RegisterType) -> RiscVRegister {
        let reg = self.ask(r_type);
        self.occupy(reg);
        reg
    }

    pub fn address(&self, ptr: &ValueData) -> Option<&Address> {
        self.map.get(&as_ptr!(ptr))
    }

    fn frame_size_mut(&mut self) -> Result<&mut usize, AsmError> {
        self.frame_sizes
            .back_mut()
            .ok_or(AsmError::InvalidStackFrame)
    }

    fn frame_size(&self) -> Result<usize, AsmError> {
        self.frame_sizes
            .back()
            .copied()
            .ok_or(AsmError::InvalidStackFrame)
    }

    /// Create a new pointer and allocate memory for it.
    pub fn new(&mut self, ptr: &ValueData) -> Result<(), AsmError> {
        // Always save the data in the stack.
        // TODO: Optimize the register usage.
        self.malloc(ptr, 4)
    }

    /// Allocate register for the data.
    pub fn ralloc(&mut self, ptr: &ValueData) {
        let reg = self.dispatch(RegisterType::Local);
        let address = Address::Register(reg);
        self.map.insert(as_ptr!(ptr), address);
    }

    /// Allocate memory for the data.
    pub fn malloc(&mut self, ptr: &ValueData, size: usize) -> Result<(), AsmError> {
        let offset = self.frame_size()? as i32;
        let address = Address::Stack(Stack { offset });
        *self.frame_size_mut()? += size;

        self.map.insert(as_ptr!(ptr), address);
        Ok(())
    }

    pub fn free(&mut self, ptr: &ValueData) {
        self.map.remove(&as_ptr!(ptr));
    }

    pub fn new_frame(&mut self, asm: &mut AsmProgram) {
        self.frame_sizes.push_back(0);

        // add a placeholder here, waiting for update when the frame size is known.
        asm.push(Inst::Placeholder);
    }

    pub fn end_frame(&mut self, asm: &mut AsmProgram) -> Result<(), AsmError> {
        let size = self
            .frame_sizes
            .pop_back()
            .ok_or(AsmError::InvalidStackFrame)?;
        // align to 16
        let size = ((size + 15) & !15) as i32;

        // get into stack
        let mut flag = false;
        for inst in asm.iter_mut().rev() {
            if let Inst::Placeholder = inst {
                flag = true;
                if size == 0 {
                    *inst = Inst::Nop;
                    return Ok(());
                } else {
                    *inst = Inst::Addi(Addi(&SP, &SP, -size));
                }
                break;
            }
        }
        if !flag {
            return Err(AsmError::InvalidStackFrame);
        }
        // get out of stack
        asm.push(Inst::Addi(Addi(&SP, &SP, size)));
        Ok(())
    }

    /// Save data in the register to the address of the dest.
    pub fn save(
        &mut self,
        dest: &ValueData,
        register: RiscVRegister,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let address = self
            .address(dest)
            .ok_or(AsmError::RegisterNotAssigned(dest.name().clone()))?;

        match address {
            Address::Register(reg) => {
                // move the data from the register to the register
                if *reg == register {
                    return Ok(());
                }
                asm.push(Inst::Mv(Mv(register, *reg)));
            }
            Address::Stack(stack) => {
                // save the data in the register to the stack
                let offset = stack.offset;
                asm.push(Inst::Sw(Sw(register, &SP, offset)));
            }
        }

        // FIXME: decide whether to free the register here
        self.release(register);
        Ok(())
    }

    /// load the data from the address of the src to the register.
    pub fn load(&mut self, src: &ValueData, asm: &mut AsmProgram) -> Option<RiscVRegister> {
        match self.map.get(&as_ptr!(src))? {
            // cannot reach register here before we finish the optimization
            Address::Register(register) => Some(*register),
            Address::Stack(stack) => {
                // load the data from the stack
                let offset = stack.offset;
                let reg = self.dispatch(RegisterType::Local);
                asm.push(Inst::Lw(Lw(reg, &SP, offset)));
                Some(reg)
            }
        }
    }

    /// load the data from the address of the src to the register.
    pub fn load_or_error(
        &mut self,
        src: &ValueData,
        asm: &mut AsmProgram,
    ) -> Result<RiscVRegister, AsmError> {
        self.load(src, asm)
            .ok_or(AsmError::RegisterNotAssigned(src.name().clone()))
    }
}
