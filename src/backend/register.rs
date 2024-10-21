use super::{instruction::*, AsmError};
use koopa::ir::entities::ValueData;
use koopa::ir::values::Call;
use koopa::ir::{FunctionData, ValueKind};
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet, LinkedList};
use std::fmt;

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
}

impl RegisterType {
    const ARGU_REGISTERS: [RiscVRegister; 8] = [A0, A1, A2, A3, A4, A5, A6, A7];
    const SAVED_REGISTERS: [RiscVRegister; 11] = [S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11];
    const TEMP_REGISTERS: [RiscVRegister; 7] = [T0, T1, T2, T3, T4, T5, T6];

    pub fn all(&self) -> Vec<RiscVRegister> {
        match self {
            RegisterType::Argument => Self::ARGU_REGISTERS.to_vec(),
            RegisterType::Saved => Self::SAVED_REGISTERS.to_vec(),
            RegisterType::Temp => Self::TEMP_REGISTERS.to_vec(),
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
    map: HashMap<Pointer, Location>,
    frames: LinkedList<Frame>,
    used: HashSet<RiscVRegister>,
}

#[derive(Debug, Default)]
pub struct Frame {
    var_size: usize,
    has_call: bool,
    param_bias: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Location {
    Register(RiscVRegister),
    Stack(Stack),
}

/// Stack address, which grows from high to low.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack {
    offset: i32, // the offset from the stack pointer
}

impl RegisterDispatcher {
    const STACK_IN: u8 = 0;
    const STACK_OUT: u8 = 1;
    const SAVE_RA: u8 = 2;
    const RECOVER_RA: u8 = 3;

    const MAX_PARAM_REG: usize = 8;
    const INT_SIZE: usize = 4;

    /// Ask for a register for the data.
    pub fn ask(&self, r_type: RegisterType) -> RiscVRegister {
        for reg in r_type.all() {
            if self.askfor(reg) {
                return reg;
            }
        }
        todo!()
    }

    /// Ask if the register is available.
    pub fn askfor(&self, register: RiscVRegister) -> bool {
        !self.used.contains(register)
    }

    /// Occupy the register.
    pub fn occupy(&mut self, register: RiscVRegister) {
        self.used.insert(register);
    }

    /// Release the register.
    pub fn release(&mut self, register: RiscVRegister) {
        self.used.remove(register);
    }

    /// Ask, occupy and return the register.
    pub fn dispatch(&mut self, r_type: RegisterType) -> RiscVRegister {
        let reg = self.ask(r_type);
        self.occupy(reg);
        reg
    }

    /// Get the location of the data.
    pub fn location(&self, ptr: &ValueData) -> Option<&Location> {
        self.map.get(&as_ptr!(ptr))
    }

    pub fn location_or_error(&self, ptr: &ValueData) -> Result<&Location, AsmError> {
        self.location(ptr)
            .ok_or(AsmError::NullLocation(ptr.name().clone()))
    }

    /// Get the current frame.
    fn current_frame_mut(&mut self) -> Result<&mut Frame, AsmError> {
        self.frames.back_mut().ok_or(AsmError::InvalidStackFrame)
    }

    /// Get the current frame.
    fn current_frame(&self) -> Result<&Frame, AsmError> {
        self.frames.back().ok_or(AsmError::InvalidStackFrame)
    }

    /// Create a new pointer and allocate memory for it.
    pub fn new(&mut self, ptr: &ValueData) -> Result<(), AsmError> {
        // Always save the data in the stack.
        // TODO: Optimize the register usage.
        self.malloc(ptr, Self::INT_SIZE)
    }

    /// Allocate register for the data.
    pub fn ralloc(&mut self, ptr: &ValueData) {
        let reg = self.dispatch(RegisterType::Temp);
        let address = Location::Register(reg);
        self.map.insert(as_ptr!(ptr), address);
    }

    /// Allocate memory for the data.
    pub fn malloc(&mut self, ptr: &ValueData, size: usize) -> Result<(), AsmError> {
        let frame = self.current_frame()?;
        let offset = (frame.var_size + frame.param_bias) as i32;
        let address = Location::Stack(Stack { offset });
        self.current_frame_mut()?.var_size += size;

        self.map.insert(as_ptr!(ptr), address);
        Ok(())
    }

    /// Free the memory for the data.
    pub fn free(&mut self, ptr: &ValueData) {
        self.map.remove(&as_ptr!(ptr));
    }

    /// Start a new frame.
    pub fn new_frame(&mut self, func_data: &FunctionData, asm: &mut AsmProgram) {
        asm.push(Inst::Comment("-- prologue".to_string()));
        asm.push(Inst::Placeholder(Self::STACK_IN));

        let mut frame = Frame::default();

        // calculate the parameter bias
        for (_, node) in func_data.layout().bbs() {
            for inst in node.insts().keys() {
                let data = func_data.dfg().value(*inst);
                if let ValueKind::Call(call) = data.kind() {
                    frame.has_call = true;
                    let p_num = call.args().len();
                    if p_num > Self::MAX_PARAM_REG {
                        frame.param_bias = max(
                            frame.param_bias,
                            Self::INT_SIZE * (p_num - Self::MAX_PARAM_REG),
                        );
                    }
                }
            }
        }
        if frame.has_call {
            asm.push(Inst::Placeholder(Self::SAVE_RA));
        }

        self.frames.push_back(frame);
        // add a placeholder here, waiting for update when the frame size is known.
    }

    /// Mark an exit of the frame.
    pub fn out_frame(&mut self, asm: &mut AsmProgram) -> Result<(), AsmError> {
        // read all "call" instructions in the function
        asm.push(Inst::Comment("-- epilogue".to_string()));
        if self.current_frame()?.has_call {
            asm.push(Inst::Placeholder(Self::RECOVER_RA));
        }
        asm.push(Inst::Placeholder(Self::STACK_OUT));
        Ok(())
    }

    /// End the frame.
    pub fn end_frame(&mut self, asm: &mut AsmProgram) -> Result<(), AsmError> {
        let frame = self.frames.pop_back().ok_or(AsmError::InvalidStackFrame)?;
        let size = frame.var_size + (frame.has_call as usize * Self::INT_SIZE) + frame.param_bias;

        // align to 16
        let size = ((size + 15) & !15) as i32;

        // recover the place holder
        let mut flag = false;
        for inst in asm.iter_mut().rev() {
            if let Inst::Placeholder(p) = inst {
                match *p {
                    Self::STACK_IN => {
                        flag = true;
                        if size == 0 {
                            *inst = Inst::Nop;
                            break;
                        } else {
                            *inst = Inst::Addi(Addi(&SP, &SP, -size));
                        }
                        break;
                    }
                    Self::STACK_OUT => {
                        *inst = if size == 0 {
                            Inst::Nop
                        } else {
                            Inst::Addi(Addi(&SP, &SP, size))
                        }
                    }
                    Self::SAVE_RA => {
                        *inst = Inst::Sw(Sw(&RA, &SP, size - Self::INT_SIZE as i32));
                    }
                    Self::RECOVER_RA => {
                        *inst = Inst::Lw(Lw(&RA, &SP, size - Self::INT_SIZE as i32));
                    }
                    _ => unreachable!(),
                }
            }
        }
        if !flag {
            return Err(AsmError::InvalidStackFrame);
        }
        Ok(())
    }

    /// Save data in the register to the address of the dest.
    pub fn save_val_to(
        &mut self,
        dest: &ValueData,
        src: RiscVRegister,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        match self.location_or_error(dest)? {
            Location::Register(reg) => {
                if *reg == src {
                    return Ok(());
                }
                asm.push(Inst::Mv(Mv(*reg, src)));
            }
            Location::Stack(stack) => {
                // save the data in the register to the stack
                let offset = stack.offset;
                asm.push(Inst::Sw(Sw(src, &SP, offset)));
            }
        }

        // FIXME: decide whether to free the register here
        self.release(src);
        Ok(())
    }

    pub fn load_imm_to(&mut self, imm: i32, dest: RiscVRegister, asm: &mut AsmProgram) {
        asm.push(Inst::Li(Li(dest, imm)));
    }

    pub fn load_imm(&mut self, imm: i32, asm: &mut AsmProgram) -> Result<RiscVRegister, AsmError> {
        if imm == 0 {
            // simple optimization for zero
            return Ok(ZERO);
        }
        let reg = self.dispatch(RegisterType::Temp);
        self.load_imm_to(imm, reg, asm);
        Ok(reg)
    }

    pub fn load_val_to(
        &mut self,
        src: &ValueData,
        dest: RiscVRegister,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        self.occupy(dest);
        match self.location_or_error(src)? {
            Location::Register(register) => {
                if *register == dest {
                    return Ok(());
                }
                asm.push(Inst::Mv(Mv(dest, *register)));
            }
            Location::Stack(stack) => {
                let offset = stack.offset;
                asm.push(Inst::Lw(Lw(dest, &SP, offset)));
            }
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_val(
        &mut self,
        src: &ValueData,
        asm: &mut AsmProgram,
    ) -> Result<RiscVRegister, AsmError> {
        match self.location_or_error(src)? {
            // cannot reach register here before we finish the optimization
            Location::Register(register) => Ok(*register),
            Location::Stack(_) => {
                // load the data from the stack
                let reg = self.dispatch(RegisterType::Temp);
                self.load_val_to(src, reg, asm)?;
                Ok(reg)
            }
        }
    }

    fn param_location(index: usize) -> Location {
        if index < Self::MAX_PARAM_REG {
            Location::Register(RegisterType::ARGU_REGISTERS[index])
        } else {
            let offset = ((index - Self::MAX_PARAM_REG) * Self::INT_SIZE) as i32;
            Location::Stack(Stack { offset })
        }
    }

    pub fn load_func_params(
        &mut self,
        params: Vec<&ValueData>,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        for (i, &param) in params.iter().enumerate() {
            let location = match Self::param_location(i) {
                Location::Register(reg) => Location::Register(reg),
                Location::Stack(stack) => todo!(),
            };
            self.map.insert(as_ptr!(param), location);
        }
        Ok(())
    }

    pub fn save_func_params(
        &mut self,
        params: Vec<&ValueData>,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        for (i, &param) in params.iter().enumerate() {
            match Self::param_location(i) {
                Location::Register(reg) => match param.kind() {
                    ValueKind::Integer(int) => self.load_imm_to(int.value(), reg, asm),
                    _ => self.load_val_to(param, reg, asm)?,
                },
                Location::Stack(stack) => {
                    let temp = match param.kind() {
                        ValueKind::Integer(int) => self.load_imm(int.value(), asm)?,
                        _ => self.load_val(param, asm)?,
                    };
                    let offset = stack.offset;
                    asm.push(Inst::Sw(Sw(temp, &SP, offset)));
                    self.release(temp);
                }
            };
        }
        Ok(())
    }
}
