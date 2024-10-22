use super::{instruction::*, AsmError};
use koopa::ir::entities::ValueData;
use koopa::ir::{FunctionData, ValueKind};
use std::cmp::max;
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

/// Return address
pub const RA: RiscVRegister = &Register { name: "ra" };

/// Frame pointer
pub const S0: RiscVRegister = &Register { name: "s0" };

pub enum RegisterType {
    Saved, // callee-saved
    Temp,  // caller-saved
}

impl RegisterType {
    const ARGU_REGISTERS: [RiscVRegister; 8] = [A0, A1, A2, A3, A4, A5, A6, A7];
    const SAVED_REGISTERS: [RiscVRegister; 11] = [S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11];
    const TEMP_REGISTERS: [RiscVRegister; 7] = [T0, T1, T2, T3, T4, T5, T6];

    pub fn all(&self) -> Vec<RiscVRegister> {
        match self {
            RegisterType::Saved => Self::SAVED_REGISTERS.to_vec(),
            RegisterType::Temp => Self::TEMP_REGISTERS.to_vec(),
        }
    }
}

pub type Pointer = *const ValueData;

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
enum Location {
    Register(RiscVRegister),
    Stack(Stack),
    Data(Data),
}

/// Stack address, which grows from high to low.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Stack {
    base: RiscVRegister,
    offset: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Data {
    label: Label,
    offset: i32,
}

/// Basic element in an instruction, including immediate value and register.
pub enum AsmElement {
    Local(Pointer),
    Imm(i32),
}

impl AsmElement {
    pub fn from(data: &ValueData) -> Self {
        match data.kind() {
            ValueKind::Integer(int) => AsmElement::Imm(int.value()),
            _ => AsmElement::Local(data),
        }
    }
}

pub const MAX_PARAM_REG: usize = 8;
pub const INT_SIZE: usize = 4;

impl RegisterDispatcher {
    const SP_IN: u8 = 0;
    const SP_OUT: u8 = 1;
    const SAVE_RA: u8 = 2;
    const RECOVER_RA: u8 = 3;
    const SAVE_S0: u8 = 4;
    const RECOVER_S0: u8 = 5;

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
    fn location(&self, ptr: Pointer) -> Option<&Location> {
        self.map.get(&ptr)
    }

    fn location_or_error(&self, ptr: Pointer) -> Result<&Location, AsmError> {
        self.location(ptr).ok_or(AsmError::NullLocation(None))
    }

    /// Get the current frame.
    fn current_frame_mut(&mut self) -> Result<&mut Frame, AsmError> {
        self.frames.back_mut().ok_or(AsmError::InvalidStackFrame)
    }

    /// Get the current frame.
    fn current_frame(&self) -> Result<&Frame, AsmError> {
        self.frames.back().ok_or(AsmError::InvalidStackFrame)
    }

    pub fn global_new(&mut self, ptr: Pointer, label: Label) {
        self.map
            .insert(ptr, Location::Data(Data { label, offset: 0 }));
    }

    /// Create a new pointer and allocate memory for it.
    pub fn new_val(&mut self, ptr: Pointer) -> Result<(), AsmError> {
        // Always save the data in the stack.
        // TODO: Malloc or Register? That's a question
        self.malloc(ptr, INT_SIZE)
    }

    /// Allocate register for the data.
    pub fn ralloc(&mut self, ptr: Pointer) {
        let reg = self.dispatch(RegisterType::Temp);
        let address = Location::Register(reg);
        self.map.insert(ptr, address);
    }

    /// Allocate memory for the data.
    pub fn malloc(&mut self, ptr: Pointer, size: usize) -> Result<(), AsmError> {
        let frame = self.current_frame()?;
        let offset = (frame.var_size + frame.param_bias) as i32;
        let address = Location::Stack(Stack { base: SP, offset });
        self.current_frame_mut()?.var_size += size;

        self.map.insert(ptr, address);
        Ok(())
    }

    /// Free the memory for the data.
    pub fn free(&mut self, ptr: Pointer) {
        self.map.remove(&ptr);
    }

    fn need_save_ra(&self) -> bool {
        self.current_frame().map_or(false, |frame| frame.has_call)
    }

    fn need_use_s0(&self) -> bool {
        self.current_frame()
            .map_or(false, |frame| frame.param_bias > 0)
    }

    /// Start a new frame.
    pub fn new_frame(&mut self, func_data: &FunctionData, asm: &mut AsmProgram) {
        asm.push(Inst::Comment("-- prologue".to_string()));
        // add a placeholder here, waiting for update when the frame size is known.
        asm.push(Inst::Placeholder(Self::SP_IN));

        let mut frame = Frame::default();

        // calculate the parameter bias
        for (_, node) in func_data.layout().bbs() {
            for inst in node.insts().keys() {
                let data = func_data.dfg().value(*inst);
                if let ValueKind::Call(call) = data.kind() {
                    frame.has_call = true;
                    let p_num = call.args().len();
                    if p_num > MAX_PARAM_REG {
                        frame.param_bias =
                            max(frame.param_bias, INT_SIZE * (p_num - MAX_PARAM_REG));
                    }
                }
            }
        }
        self.frames.push_back(frame);

        if self.need_save_ra() {
            asm.push(Inst::Placeholder(Self::SAVE_RA));
        }
        if self.need_use_s0() {
            asm.push(Inst::Placeholder(Self::SAVE_S0));
            asm.push(Inst::Mv(Mv(S0, SP)));
        }
    }

    /// Mark an exit of the frame.
    pub fn out_frame(&mut self, asm: &mut AsmProgram) -> Result<(), AsmError> {
        // read all "call" instructions in the function
        asm.push(Inst::Comment("-- epilogue".to_string()));
        if self.need_save_ra() {
            asm.push(Inst::Placeholder(Self::RECOVER_RA));
        }
        if self.need_use_s0() {
            asm.push(Inst::Placeholder(Self::RECOVER_S0));
        }
        asm.push(Inst::Placeholder(Self::SP_OUT));
        Ok(())
    }

    /// End the frame.
    pub fn end_frame(&mut self, asm: &mut AsmProgram) -> Result<(), AsmError> {
        let frame = self.frames.pop_back().ok_or(AsmError::InvalidStackFrame)?;
        let mut size = frame.var_size + frame.param_bias;
        if self.need_save_ra() {
            size += INT_SIZE;
        }
        if self.need_use_s0() {
            size += INT_SIZE;
        }

        // align to 16
        let size = ((size + 15) & !15) as i32;

        // recover the place holder
        let mut flag = false;
        for inst in asm.iter_mut().rev() {
            if let Inst::Placeholder(p) = inst {
                match *p {
                    Self::SP_IN => {
                        flag = true;
                        if size == 0 {
                            *inst = Inst::Nop;
                        } else {
                            *inst = Inst::Addi(Addi(SP, SP, -size));
                        }
                        break;
                    }
                    Self::SP_OUT => {
                        *inst = if size == 0 {
                            Inst::Nop
                        } else {
                            Inst::Addi(Addi(SP, SP, size))
                        }
                    }
                    Self::SAVE_RA => {
                        *inst = Inst::Sw(Sw(RA, SP, size - INT_SIZE as i32));
                    }
                    Self::RECOVER_RA => {
                        *inst = Inst::Lw(Lw(RA, SP, size - INT_SIZE as i32));
                    }
                    Self::SAVE_S0 => {
                        *inst = Inst::Sw(Sw(S0, SP, size - 2 * INT_SIZE as i32));
                    } 
                    Self::RECOVER_S0 => {
                        *inst = Inst::Lw(Lw(S0, SP, size - 2 * INT_SIZE as i32));
                    }
                    _ => unreachable!("Unknown placeholder"),
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
        dest: Pointer,
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
                asm.push(Inst::Sw(Sw(src, stack.base, stack.offset)));
            }
            Location::Data(data) => {
                let label = data.label.clone();
                let offset = data.offset;
                let reg = self.dispatch(RegisterType::Temp);
                asm.push(Inst::La(La(reg, label)));
                asm.push(Inst::Sw(Sw(src, reg, offset)));
                self.release(reg);
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
        src: Pointer,
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
                asm.push(Inst::Lw(Lw(dest, stack.base, stack.offset)));
            }
            Location::Data(data) => {
                asm.push(Inst::La(La(dest, data.label.clone())));
                asm.push(Inst::Lw(Lw(dest, dest, data.offset)));
            }
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_val(
        &mut self,
        src: Pointer,
        asm: &mut AsmProgram,
    ) -> Result<RiscVRegister, AsmError> {
        match self.location_or_error(src)? {
            // cannot reach register here before we finish the optimization
            Location::Register(register) => Ok(*register),
            _ => {
                // In memory
                let reg = self.dispatch(RegisterType::Temp);
                self.load_val_to(src, reg, asm)?;
                Ok(reg)
            }
        }
    }

    pub fn load_to(
        &mut self,
        element: &AsmElement,
        dest: RiscVRegister,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        match element {
            AsmElement::Local(ptr) => self.load_val_to(*ptr, dest, asm),
            AsmElement::Imm(imm) => Ok(self.load_imm_to(*imm, dest, asm)),
        }
    }

    pub fn load(
        &mut self,
        element: &AsmElement,
        asm: &mut AsmProgram,
    ) -> Result<RiscVRegister, AsmError> {
        match element {
            AsmElement::Local(ptr) => self.load_val(*ptr, asm),
            AsmElement::Imm(imm) => Ok(self.load_imm(*imm, asm)?),
        }
    }

    fn param_location(index: usize) -> Location {
        if index < MAX_PARAM_REG {
            Location::Register(RegisterType::ARGU_REGISTERS[index])
        } else {
            let offset = ((index - MAX_PARAM_REG) * INT_SIZE) as i32;
            Location::Stack(Stack { base: SP, offset })
        }
    }

    pub fn load_func_param(
        &mut self,
        index: usize,
        param: &ValueData,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let location = match Self::param_location(index) {
            Location::Register(reg) => Location::Register(reg),
            Location::Stack(stack) => {
                // the params are in the stack of the caller
                // copy them to the stack of the callee
                let offset = stack.offset;
                Location::Stack(Stack { base: S0, offset })
            }
            Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        };
        self.map.insert(param, location);
        Ok(())
    }

    pub fn save_func_param(
        &mut self,
        index: usize,
        param: &ValueData,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        // TODO: Save local variables to the stack/callee-saved registers
        let e = AsmElement::from(param);
        match Self::param_location(index) {
            Location::Register(reg) => self.load_to(&e, reg, asm)?,
            Location::Stack(stack) => {
                let temp = self.load(&e, asm)?;
                asm.push(Inst::Sw(Sw(temp, stack.base, stack.offset)));
                self.release(temp);
            }
            Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        }
        Ok(())
    }
}
