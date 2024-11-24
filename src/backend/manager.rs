use super::instruction::*;
use super::location::{AsmElement, Data, Location, Pointer, Stack, ToLocation};
use super::program::{AsmGlobal, AsmLocal, AsmProgram};
use super::registers::*;
use super::{AsmError, INT_SIZE, MAX_PARAM_REG};
use crate::utils::namer::original_ident;
use koopa::ir::FunctionData;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct PointerInfo {
    /// where the data is stored
    pub location: Location,
}

#[derive(Debug, Default)]
pub struct AsmManager {
    map: HashMap<Pointer, PointerInfo>,
    glb_map: HashMap<Pointer, PointerInfo>,
    reg_index: usize,
    func_index: usize,
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

impl AsmManager {
    pub fn add_loc(&mut self, ptr: Pointer, loc: Location) {
        self.map.insert(ptr, PointerInfo { location: loc });
    }

    pub fn add_glb_loc(&mut self, ptr: Pointer, loc: Location) {
        self.glb_map.insert(ptr, PointerInfo { location: loc });
    }

    pub fn loc(&self, ptr: Pointer) -> Result<Location, AsmError> {
        self.where_is(ptr)
            .ok_or(AsmError::NullLocation(Some(format!("{ptr:?}"))))
    }

    pub fn where_is(&self, ptr: Pointer) -> Option<Location> {
        self.map.get(&ptr).map(|info| info.location.clone())
    }

    pub fn new_func(&mut self, func_data: &FunctionData, asm: &mut AsmProgram) {
        let label = original_ident(&func_data.name().to_string());
        self.func_index += 1;
        let mut glb = AsmGlobal::new(Directive::Text, label);
        glb.new_local(AsmLocal::new(Some(format!(
            ".prologue_{}",
            self.func_index
        ))));
        asm.new_global(glb);
    }

    pub fn end_func(&mut self) {
        self.map = self.glb_map.clone();
    }

    pub fn global_new(&mut self, ptr: Pointer, label: Label) {
        let loc = Data { label, offset: 0 }.to_loc();
        self.add_loc(ptr, loc.clone());
        self.add_glb_loc(ptr, loc);
    }

    pub fn new_reg(&mut self) -> Register {
        self.reg_index += 1;
        Register::Fake(FakeRegister(self.reg_index - 1))
    }

    pub fn new_val(&mut self, ptr: Pointer, asm: &mut AsmProgram) -> Register {
        let reg = self.new_reg();
        self.add_loc(ptr, reg.to_loc());
        reg
    }

    pub fn new_val_with_src(
        &mut self,
        ptr: Pointer,
        src: &RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let address = src.reg.to_loc();
        self.add_loc(ptr, address);
        self.save_val_to(ptr, src, asm)
    }

    /// Save data in the register to the address of the dest.
    pub fn save_val_to(
        &mut self,
        dest: Pointer,
        src: &RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let src = src.reg;
        match &self.loc(dest)? {
            Location::Register(reg) => {
                if *reg == src {
                    return Ok(());
                }
                asm.push(Inst::Mv(*reg, src));
            }
            Location::Stack(stack) => {
                // save the data in the register to the stack
                asm.push(Inst::Sw(src, stack.base, stack.offset));
            }
            Location::Data(data) => {
                let label = data.label.clone();
                asm.push(Inst::La(FREE_REG, label));
                asm.push(Inst::Sw(src, FREE_REG, data.offset));
            }
        }
        Ok(())
    }

    pub fn load_imm_to(
        &mut self,
        imm: i32,
        dest: &mut RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        // We never reuse immediate, only load it to the register.
        let dest_reg = dest.reg;
        asm.push(Inst::Li(dest_reg, imm));
        dest.reg = dest_reg;
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
                asm.push(Inst::Addi(dest, stack.base, stack.offset));
            }
            Location::Data(data) => {
                asm.push(Inst::La(dest, data.label.clone()));
                asm.push(Inst::Addi(dest, dest, data.offset));
            }
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_deref_to(&mut self, src: &RegPack, dest: &mut RegPack, asm: &mut AsmProgram) {
        asm.push(Inst::Lw(dest.reg, src.reg, 0));
    }

    /// Save the data in the register to the address of the dest.
    pub fn save_deref_to(&mut self, src: &RegPack, dest: &mut RegPack, asm: &mut AsmProgram) {
        asm.push(Inst::Sw(src.reg, dest.reg, 0));
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

                let mut pack = RegPack::new(self.new_reg());
                self.load_imm_to(unit_size as i32, &mut pack, asm)?;
                let unit = pack.reg;

                asm.push(Inst::Mul(FREE_REG, FREE_REG, unit));
                asm.push(Inst::Add(reg, reg, FREE_REG));
            }
            AsmElement::Imm(imm) => {
                asm.push(Inst::Addi(reg, reg, imm * (unit_size as i32)));
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
        let dest_reg = dest.reg;
        match &self.loc(src)? {
            Location::Register(reg) => {
                if *reg != dest_reg {
                    asm.push(Inst::Mv(dest_reg, *reg));
                    dest.reg = dest_reg;
                }
            }
            Location::Stack(stack) => {
                asm.push(Inst::Lw(dest_reg, stack.base, stack.offset));
                dest.reg = dest_reg;
            }
            Location::Data(data) => {
                asm.push(Inst::La(dest_reg, data.label.clone()));
                asm.push(Inst::Addi(dest_reg, dest_reg, data.offset));
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
            RegisterType::all(&RegisterType::Arg)[index].to_loc()
        } else {
            let offset = ((index - MAX_PARAM_REG) * INT_SIZE) as i32;
            Stack { base: SP, offset }.to_loc()
        }
    }

    pub fn load_func_param(&mut self, index: usize, param: Pointer) -> Result<(), AsmError> {
        let location = match Self::param_location(index) {
            Location::Register(reg) => reg.to_loc(),
            Location::Stack(stack) => {
                // the params are in the stack of the caller
                let offset = stack.offset;
                Stack { base: S0, offset }.to_loc()
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
                asm.push(Inst::Sw(FREE_REG, stack.base, stack.offset));
            }
            Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        }
        Ok(())
    }
}
