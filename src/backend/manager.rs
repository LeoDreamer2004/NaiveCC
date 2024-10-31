use koopa::ir::FunctionData;
use koopa::ir::{entities::ValueData, ValueKind};
use std::cmp::max;
use std::collections::{HashMap, LinkedList};

use super::instruction::*;
use super::register::*;
use super::AsmError;

pub type Pointer = *const ValueData;

#[derive(Debug, Default)]
pub struct AsmManager {
    map: HashMap<Pointer, DataInfo>,
    frames: LinkedList<Frame>,
    dpt: RegisterDispatcher,
}

#[derive(Debug, Default)]
pub struct Frame {
    var_size: usize,
    has_call: bool,
    param_bias: usize,
}

#[derive(Debug, Clone)]
pub struct DataInfo {
    /// where the data is stored
    location: Location,
    /// if the data is a pointer to another data
    ///
    /// If so, when this pointer is loaded or stored,
    /// we actually load or store the data it points to
    is_ptr: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Location {
    /// The data is in the register.
    Register(Register),
    /// The data is in the stack.
    Stack(Stack),
    /// The data is in the data section.
    Data(Data),
}

/// Stack address, which grows from high to low.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Stack {
    base: Register,
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
    pub fn from(data: Pointer) -> Self {
        unsafe {
            match data.as_ref().unwrap().kind() {
                ValueKind::Integer(int) => AsmElement::Imm(int.value()),
                ValueKind::ZeroInit(_) => AsmElement::Imm(0),
                _ => AsmElement::Local(data),
            }
        }
    }
}

pub const MAX_PARAM_REG: usize = 8;
pub const INT_SIZE: usize = 4;
pub const MAX_STACK_SIZE: usize = 1 << 22;

impl AsmManager {
    const SP_IN: u8 = 0;
    const SP_OUT: u8 = 1;
    const SAVE_RA: u8 = 2;
    const RECOVER_RA: u8 = 3;
    const SAVE_S0: u8 = 4;
    const RECOVER_S0: u8 = 5;

    pub fn dpt(&mut self) -> &mut RegisterDispatcher {
        &mut self.dpt
    }

    /// Get the location of the data.
    fn info(&self, ptr: Pointer) -> Option<&DataInfo> {
        self.map.get(&ptr)
    }

    fn try_info(&self, ptr: Pointer) -> Result<&DataInfo, AsmError> {
        self.info(ptr).ok_or(AsmError::NullLocation(None))
    }

    /// Get the current frame.
    fn current_frame_mut(&mut self) -> Result<&mut Frame, AsmError> {
        self.frames.back_mut().ok_or(AsmError::InvalidStackFrame)
    }

    /// Get the current frame.
    fn current_frame(&self) -> Result<&Frame, AsmError> {
        self.frames.back().ok_or(AsmError::InvalidStackFrame)
    }

    fn add_location(&mut self, ptr: Pointer, location: Location) {
        self.map.insert(
            ptr,
            DataInfo {
                location,
                is_ptr: false,
            },
        );
    }

    pub fn mark_as_ptr(&mut self, ptr: Pointer) {
        if let Some(info) = self.map.get_mut(&ptr) {
            info.is_ptr = true;
        }
    }

    pub fn global_new(&mut self, ptr: Pointer, label: Label) {
        self.add_location(ptr, Location::Data(Data { label, offset: 0 }));
    }

    /// Create a new pointer and allocate memory for it.
    pub fn new_val(&mut self, ptr: Pointer) -> Result<(), AsmError> {
        // Always save the data in the stack.
        // TODO: Malloc or Register? That's a question
        self.malloc(ptr, INT_SIZE)
    }

    /// Allocate register for the data.
    pub fn ralloc(&mut self, ptr: Pointer) {
        let reg = self.dpt.dispatch(RegisterType::Temp);
        let address = Location::Register(reg);
        self.add_location(ptr, address);
    }

    /// Allocate memory for the data.
    pub fn malloc(&mut self, ptr: Pointer, size: usize) -> Result<(), AsmError> {
        let frame = self.current_frame_mut()?;
        let offset = (frame.var_size + frame.param_bias) as i32;
        let address = Location::Stack(Stack { base: SP, offset });
        frame.var_size += size;

        self.add_location(ptr, address);
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
        if size > MAX_STACK_SIZE as i32 {
            return Err(AsmError::StackOverflow);
        }

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
        src: Register,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        if self.try_info(dest)?.is_ptr {
            // just load the data of the pointer, and store the data in the register
            self.load_raw_val_to(dest, FREE_REG, asm)?;
            asm.push(Inst::Sw(Sw(src, FREE_REG, 0)));
            // FIXME: decide whether to free the register here
            self.dpt.release(src);
            return Ok(());
        }

        let info = self.try_info(dest)?;

        match &info.location.clone() {
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
                asm.push(Inst::La(La(FREE_REG, label)));
                asm.push(Inst::Sw(Sw(src, FREE_REG, data.offset)));
            }
        }

        // FIXME: decide whether to free the register here
        self.dpt.release(src);
        Ok(())
    }

    /// TODO: Merge the two functions
    /// such as load_imm(&mut self, imm: i32, dest: Option<RiscVRegister>, asm: &mut AsmProgram) -> Result<RiscVRegister, AsmError>

    pub fn load_imm_to(&mut self, imm: i32, dest: Register, asm: &mut AsmProgram) {
        asm.push(Inst::Li(Li(dest, imm)));
    }

    pub fn load_imm(&mut self, imm: i32, asm: &mut AsmProgram) -> Result<Register, AsmError> {
        if imm == 0 {
            // simple optimization for zero
            return Ok(ZERO);
        }
        let reg = self.dpt.dispatch(RegisterType::Temp);
        self.load_imm_to(imm, reg, asm);
        Ok(reg)
    }

    pub fn load_address_to(
        &mut self,
        src: Pointer,
        bias: &AsmElement,
        unit_size: usize,
        dest: Register,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        self.dpt.occupy(dest);
        let info = self.try_info(src)?;

        let (base, src_offset) = match &info.location.clone() {
            Location::Register(reg) => {
                if info.is_ptr {
                    (*reg, 0)
                } else {
                    return Err(AsmError::IllegalGetAddress);
                }
            }
            Location::Stack(stack) => {
                if info.is_ptr {
                    let reg = self.dpt.dispatch(RegisterType::Temp);
                    self.load_raw_val_to(src, reg, asm)?;
                    self.dpt.release(reg);
                    (reg, 0)
                } else {
                    (stack.base, stack.offset)
                }
            }
            Location::Data(data) => {
                asm.push(Inst::La(La(dest, data.label.clone())));
                (dest, data.offset)
            }
        };
        let (base, offset) = match bias {
            AsmElement::Local(ptr) => {
                // base += bias * unit_size
                self.load_val_to(ptr.clone(), FREE_REG, asm)?;
                self.dpt.occupy(base);
                let unit = self.load_imm(unit_size as i32, asm)?;
                self.dpt.release(base);
                asm.push(Inst::Mul(Mul(FREE_REG, FREE_REG, unit)));
                asm.push(Inst::Add(Add(dest, base, FREE_REG)));
                self.dpt.release(unit);
                (dest, src_offset)
            }
            AsmElement::Imm(imm) => (base, src_offset + imm * (unit_size as i32)),
        };
       if base != dest {
            self.dpt.release(base);
        }
        asm.push(Inst::Addi(Addi(dest, base, offset)));
        Ok(())
    }

    pub fn load_address(
        &mut self,
        src: Pointer,
        bias: &AsmElement,
        unit_size: usize,
        asm: &mut AsmProgram,
    ) -> Result<Register, AsmError> {
        let reg = self.dpt.dispatch(RegisterType::Temp);
        self.load_address_to(src, bias, unit_size, reg, asm)?;
        Ok(reg)
    }

    pub fn load_raw_val_to(
        &mut self,
        src: Pointer,
        dest: Register,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        self.dpt.occupy(dest);
        let info = self.try_info(src)?;
        match &info.location {
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

    pub fn load_val_to(
        &mut self,
        src: Pointer,
        dest: Register,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        self.load_raw_val_to(src, dest, asm)?;
        let info = self.try_info(src)?;
        if info.is_ptr {
            asm.push(Inst::Lw(Lw(dest, dest, 0)));
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_val(&mut self, src: Pointer, asm: &mut AsmProgram) -> Result<Register, AsmError> {
        let info = self.try_info(src)?;
        match &info.location {
            // cannot reach register here before we finish the optimization
            Location::Register(reg) => {
                if info.is_ptr {
                    asm.push(Inst::Lw(Lw(*reg, *reg, 0)));
                }
                Ok(*reg)
            }
            _ => {
                // In memory
                let reg = self.dpt.dispatch(RegisterType::Temp);
                self.load_val_to(src, reg, asm)?;
                Ok(reg)
            }
        }
    }

    pub fn load_to(
        &mut self,
        element: &AsmElement,
        dest: Register,
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
    ) -> Result<Register, AsmError> {
        match element {
            AsmElement::Local(ptr) => self.load_val(*ptr, asm),
            AsmElement::Imm(imm) => Ok(self.load_imm(*imm, asm)?),
        }
    }

    pub fn copy(
        &mut self,
        src: &AsmElement,
        dest: Pointer,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let reg = self.load(src, asm)?;
        self.save_val_to(dest, reg, asm)?;
        if let AsmElement::Local(ptr) = src {
            if self.try_info(ptr.clone())?.is_ptr {
                self.mark_as_ptr(dest);
            }
        }
        Ok(())
    }

    fn param_location(index: usize) -> Location {
        if index < MAX_PARAM_REG {
            Location::Register(RegisterType::ARGU_REGISTERS[index])
        } else {
            let offset = ((index - MAX_PARAM_REG) * INT_SIZE) as i32;
            Location::Stack(Stack { base: SP, offset })
        }
    }

    pub fn load_func_param(&mut self, index: usize, param: Pointer) -> Result<(), AsmError> {
        let location = match Self::param_location(index) {
            Location::Register(reg) => Location::Register(reg),
            Location::Stack(stack) => {
                // the params are in the stack of the caller
                let offset = stack.offset;
                Location::Stack(Stack { base: S0, offset })
            }
            Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        };
        self.add_location(param, location);
        Ok(())
    }

    pub fn save_func_param(
        &mut self,
        index: usize,
        param: Pointer,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        // TODO: Save local variables to the stack/callee-saved registers
        let e = AsmElement::from(param);
        match Self::param_location(index) {
            Location::Register(reg) => self.load_to(&e, reg, asm)?,
            Location::Stack(stack) => {
                self.load_to(&e, FREE_REG, asm)?;
                asm.push(Inst::Sw(Sw(FREE_REG, stack.base, stack.offset)));
            }
            Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        }
        Ok(())
    }
}
