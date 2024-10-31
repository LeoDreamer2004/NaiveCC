use koopa::ir::{entities::ValueData, ValueKind};
use std::collections::HashMap;

use super::frames::Frame;
use super::frames::FrameHelper;
use super::instruction::*;
use super::register::*;
use super::{AsmError, INT_SIZE, MAX_PARAM_REG};

pub type Pointer = *const ValueData;

#[derive(Debug, Default)]
pub struct AsmManager {
    map: HashMap<Pointer, PointerInfo>,
    dpt: RegisterDispatcher,
    fh: FrameHelper,
}

#[derive(Debug, Clone)]
pub struct RegPack {
    pub reg: Register,
    pub data: Storage,
}

impl RegPack {
    pub fn new(reg: Register) -> Self {
        Self {
            reg,
            data: Storage::Empty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PointerInfo {
    /// where the data is stored
    location: Location,
    /// what the pointer refers to
    data: Storage,
}

#[derive(Debug, Clone)]
pub enum Storage {
    Empty,
    Data,
    Location(Location),
}

impl PartialEq for Storage {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Storage::Empty, Storage::Empty) => true,
            (Storage::Location(a), Storage::Location(b)) => a == b,
            // for computer, we don't care about the data in the register
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Location {
    /// The data is in the register.
    Register(Register),
    /// The data is in the stack.
    Stack(Stack),
    /// The data is in the data section.
    Data(Data),
}

/// Stack address, which grows from high to low.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack {
    base: Register,
    offset: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Data {
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

impl AsmManager {
    pub fn dpt(&mut self) -> &mut RegisterDispatcher {
        &mut self.dpt
    }

    pub fn fh(&mut self) -> &mut FrameHelper {
        &mut self.fh
    }

    /// Get the location of the data.
    fn info(&self, ptr: Pointer) -> Result<&PointerInfo, AsmError> {
        self.map.get(&ptr).ok_or(AsmError::NullLocation(None))
    }

    fn info_mut(&mut self, ptr: Pointer) -> Result<&mut PointerInfo, AsmError> {
        self.map.get_mut(&ptr).ok_or(AsmError::NullLocation(None))
    }

    /// Get the current frame.
    fn current_frame_mut(&mut self) -> Result<&mut Frame, AsmError> {
        self.fh.frames.back_mut().ok_or(AsmError::InvalidStackFrame)
    }

    fn add_location(&mut self, ptr: Pointer, location: Location) {
        self.map.insert(
            ptr,
            PointerInfo {
                location,
                data: Storage::Empty,
            },
        );
    }

    pub fn global_new(&mut self, ptr: Pointer, label: Label) {
        self.add_location(ptr, Location::Data(Data { label, offset: 0 }));
    }

    /// Create a new pointer and allocate memory for it.
    pub fn new_val(&mut self, ptr: Pointer) -> Result<(), AsmError> {
        // Always save the data in the stack.
        // TODO: Malloc or Register? That's a question
        let address = self.malloc(INT_SIZE)?;
        self.add_location(ptr, address);
        Ok(())
    }

    /// Allocate register for the data.
    pub fn ralloc(&mut self) -> Result<Location, AsmError> {
        // let reg = self.dpt.dispatch(RegisterType::Temp);
        // let address = Location::Register(reg);
        todo!()
    }

    /// Allocate memory.
    pub fn malloc(&mut self, size: usize) -> Result<Location, AsmError> {
        let frame = self.current_frame_mut()?;
        let offset = (frame.var_size + frame.param_bias) as i32;
        let address = Location::Stack(Stack { base: SP, offset });
        frame.var_size += size;
        Ok(address)
    }

    /// Free the memory for the data.
    pub fn free(&mut self, ptr: Pointer) {
        self.map.remove(&ptr);
    }

    /// Save data in the register to the address of the dest.
    pub fn save_val_to(
        &mut self,
        dest: Pointer,
        src: &mut RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let info = self.info_mut(dest)?;
        info.data = src.data.clone();

        let src = src.reg;
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

    fn true_reg(&mut self, reg: Register) -> Register {
        if reg == ANY_REG {
            self.dpt.dispatch(RegisterType::Temp)
        } else {
            reg
        }
    }

    pub fn load_imm_to(
        &mut self,
        imm: i32,
        dest: &mut RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        // We never reuse immediate, only load it to the register.
        dest.data = Storage::Data;
        let dest_reg = dest.reg;
        if dest_reg == ANY_REG && imm == 0 {
            dest.reg = ZERO;
        } else {
            let dest_reg = self.true_reg(dest_reg);
            asm.push(Inst::Li(Li(dest_reg, imm)));
            dest.reg = dest_reg;
        }
        Ok(())
    }

    /// Build a reference to the location, and save the address to the register.
    pub fn load_ref_to(
        &mut self,
        location: Location,
        dest: &mut RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        dest.data = Storage::Location(location.clone());
        let dest = dest.reg;
        self.dpt.occupy(dest);
        match location {
            Location::Register(_) => {
                return Err(AsmError::IllegalGetAddress);
            }
            Location::Stack(stack) => {
                asm.push(Inst::Addi(Addi(dest, stack.base, stack.offset)));
            }
            Location::Data(data) => {
                asm.push(Inst::La(La(dest, data.label.clone())));
                asm.push(Inst::Addi(Addi(dest, dest, data.offset)));
            }
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_deref_to(&mut self, src: &RegPack, dest: &mut RegPack, asm: &mut AsmProgram) {
        asm.push(Inst::Lw(Lw(dest.reg, src.reg, 0)));
        dest.data = Storage::Data;
    }

    /// Save the data in the register to the address of the dest.
    pub fn save_deref_to(&mut self, src: &RegPack, dest: &mut RegPack, asm: &mut AsmProgram) {
        asm.push(Inst::Sw(Sw(src.reg, dest.reg, 0)));
        dest.data = Storage::Data;
    }

    pub fn add_bias(
        &mut self,
        pack: &mut RegPack,
        bias: &AsmElement,
        unit_size: usize,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let reg = pack.reg;
        // TODO: Pack data has been changed
        match bias {
            AsmElement::Local(ptr) => {
                self.load_val_to(ptr.clone(), &mut RegPack::new(FREE_REG), asm)?;
                self.dpt.occupy(reg);

                let mut pack = RegPack::new(ANY_REG);
                self.load_imm_to(unit_size as i32, &mut pack, asm)?;
                let unit = pack.reg;

                self.dpt.release(reg);
                asm.push(Inst::Mul(Mul(FREE_REG, FREE_REG, unit)));
                asm.push(Inst::Add(Add(reg, reg, FREE_REG)));
                self.dpt.release(unit);
            }
            AsmElement::Imm(imm) => {
                asm.push(Inst::Addi(Addi(reg, reg, *imm * (unit_size as i32))));
            }
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_val_to(
        &mut self,
        src: Pointer,
        dest: &mut RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let location = self.info(src)?.location.clone();
        dest.data = Storage::Location(location.clone());

        let dest_reg = dest.reg;
        let is_none = dest_reg == ANY_REG;
        let dest_reg = self.true_reg(dest_reg);
        self.dpt.occupy(dest_reg);
        match location {
            Location::Register(reg) => {
                dest.reg = reg;
                if is_none {
                    self.dpt.release(dest_reg);
                } else if reg != dest_reg {
                    asm.push(Inst::Mv(Mv(dest_reg, reg)));
                    dest.reg = dest_reg;
                }
            }
            Location::Stack(stack) => {
                asm.push(Inst::Lw(Lw(dest_reg, stack.base, stack.offset)));
                dest.reg = dest_reg;
            }
            Location::Data(data) => {
                asm.push(Inst::La(La(dest_reg, data.label.clone())));
                asm.push(Inst::Addi(Addi(dest_reg, dest_reg, data.offset)));
                dest.reg = dest_reg;
            }
        }
        Ok(())
    }

    pub fn load_to(
        &mut self,
        element: &AsmElement,
        dest: &mut RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        match element {
            AsmElement::Local(ptr) => self.load_val_to(*ptr, dest, asm),
            AsmElement::Imm(imm) => self.load_imm_to(*imm, dest, asm),
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
            Location::Register(reg) => {
                self.load_to(&e, &mut RegPack::new(reg), asm)?;
            }
            Location::Stack(stack) => {
                self.load_to(&e, &mut RegPack::new(FREE_REG), asm)?;
                asm.push(Inst::Sw(Sw(FREE_REG, stack.base, stack.offset)));
            }
            Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        }
        Ok(())
    }
}
