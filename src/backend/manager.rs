use std::collections::HashMap;

use crate::utils::namer::NameGenerator;

use super::frames::FrameHelper;
use super::instruction::*;
use super::registers::*;
use super::{AsmError, INT_SIZE, MAX_PARAM_REG};
use koopa::ir::{entities::ValueData, ValueKind};

pub type Pointer = *const ValueData;

type Label = String;

/// Stack address, which grows from high to low.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack {
    pub base: Register,
    pub offset: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Data {
    pub label: Label,
    pub offset: i32,
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

#[derive(Debug, Clone)]
pub struct PointerInfo {
    /// where the data is stored
    pub location: Location,
}

#[derive(Debug, Default)]
pub struct AsmManager {
    map: HashMap<Pointer, PointerInfo>,
    fh: FrameHelper,
    reg_index: u64,
}

#[derive(Debug, Clone)]
pub struct RegPack {
    pub reg: Register,
}

impl RegPack {
    pub fn new(reg: Register) -> Self {
        Self { reg }
    }
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
    pub fn add_loc(&mut self, ptr: Pointer, loc: Location) {
        self.map.insert(ptr, PointerInfo { location: loc });
    }

    pub fn info(&self, ptr: Pointer) -> Result<&PointerInfo, AsmError> {
        self.map
            .get(&ptr)
            .ok_or(AsmError::NullLocation(Some(format!("{ptr:?}"))))
    }

    pub fn info_mut(&mut self, ptr: Pointer) -> Result<&mut PointerInfo, AsmError> {
        self.map
            .get_mut(&ptr)
            .ok_or(AsmError::NullLocation(Some(format!("{ptr:?}"))))
    }

    pub fn fh(&mut self) -> &mut FrameHelper {
        &mut self.fh
    }

    pub fn global_new(&mut self, ptr: Pointer, label: Label) {
        self.add_loc(ptr, Location::Data(Data { label, offset: 0 }));
    }

    pub fn new_reg(&mut self) -> Register {
        self.reg_index += 1;
        Register::Fake(FakeRegister(self.reg_index))
    }

    pub fn new_val(&mut self, ptr: Pointer) {
        let address = Location::Register(self.new_reg());
        self.add_loc(ptr, address);
    }

    pub fn new_val_with_src(
        &mut self,
        ptr: Pointer,
        src: &RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let address = Location::Register(src.reg);
        self.add_loc(ptr, address);
        self.save_val_to(ptr, src, asm)
    }

    /// Allocate memory.
    pub fn malloc(&mut self, size: usize) -> Result<Location, AsmError> {
        let frame = self.fh.current_frame_mut()?;
        let offset = (frame.var_size + frame.param_bias) as i32;
        let address = Location::Stack(Stack { base: SP, offset });
        frame.var_size += size;
        Ok(address)
    }

    /// Save data in the register to the address of the dest.
    pub fn save_val_to(
        &mut self,
        dest: Pointer,
        src: &RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let info = self.info_mut(dest)?;

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
        Ok(())
    }

    fn true_reg(&mut self, reg: Register) -> Register {
        if reg == ANY_REG {
            self.new_reg()
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
        let dest = dest.reg;
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
    }

    /// Save the data in the register to the address of the dest.
    pub fn save_deref_to(&mut self, src: &RegPack, dest: &mut RegPack, asm: &mut AsmProgram) {
        asm.push(Inst::Sw(Sw(src.reg, dest.reg, 0)));
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

                let mut pack = RegPack::new(ANY_REG);
                self.load_imm_to(unit_size as i32, &mut pack, asm)?;
                let unit = pack.reg;

                asm.push(Inst::Mul(Mul(FREE_REG, FREE_REG, unit)));
                asm.push(Inst::Add(Add(reg, reg, FREE_REG)));
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
        let dest_reg = self.true_reg(dest.reg);
        match location {
            Location::Register(reg) => {
                if dest.reg == ANY_REG {
                    dest.reg = reg;
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
        self.add_loc(param, location);
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
