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
    pub refer: Option<Stack>,
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
    pub refer: Option<Stack>,
    pub insts: Vec<Inst>,
}

impl RegPack {
    pub fn new(reg: Register) -> Self {
        Self {
            reg,
            refer: None,
            insts: Vec::new(),
        }
    }
}

impl AsmManager {
    pub fn add_loc(&mut self, ptr: Pointer, loc: Location) {
        self.map.insert(
            ptr,
            PointerInfo {
                location: loc,
                refer: None,
            },
        );
    }

    pub fn add_glb_loc(&mut self, ptr: Pointer, loc: Location) {
        self.glb_map.insert(
            ptr,
            PointerInfo {
                location: loc,
                refer: None,
            },
        );
    }

    pub fn loc(&self, ptr: Pointer) -> Result<&Location, AsmError> {
        self.info(ptr).map(|info| &info.location)
    }

    pub fn refer(&self, ptr: Pointer) -> Result<&Option<Stack>, AsmError> {
        self.info(ptr).map(|info| &info.refer)
    }

    pub fn refer_mut(&mut self, ptr: Pointer) -> Result<&mut Option<Stack>, AsmError> {
        self.info_mut(ptr).map(|info| &mut info.refer)
    }

    fn info(&self, ptr: Pointer) -> Result<&PointerInfo, AsmError> {
        self.map
            .get(&ptr)
            .ok_or(AsmError::NullLocation(format!("{ptr:?}")))
    }

    fn info_mut(&mut self, ptr: Pointer) -> Result<&mut PointerInfo, AsmError> {
        self.map
            .get_mut(&ptr)
            .ok_or(AsmError::NullLocation(format!("{ptr:?}")))
    }

    pub fn new_func(&mut self, func_data: &FunctionData, asm: &mut AsmProgram) {
        let label = original_ident(&func_data.name().to_string());
        self.func_index += 1;
        let mut glb = AsmGlobal::new(Directive::Text, label);
        let label = format!(".prologue_{}", self.func_index);
        glb.new_local(AsmLocal::new(Some(label)));
        asm.new_global(glb);
    }

    pub fn end_func(&mut self) {
        self.map = self.glb_map.clone();
    }

    pub fn write_pack(&mut self, pack: RegPack, asm: &mut AsmProgram) {
        asm.extend(pack.insts);
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

    pub fn new_val(&mut self, ptr: Pointer) -> Register {
        let reg = self.new_reg();
        self.add_loc(ptr, reg.to_loc());
        reg
    }

    pub fn new_val_with_src(
        &mut self,
        ptr: Pointer,
        src: RegPack,
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
        src: RegPack,
        asm: &mut AsmProgram,
    ) -> Result<(), AsmError> {
        let rs = src.reg;
        *self.refer_mut(dest)? = src.refer.clone();
        self.write_pack(src, asm);

        match self.loc(dest)? {
            Location::Register(reg) => {
                if *reg == rs {
                    return Ok(());
                }
                asm.push(Inst::Mv(*reg, rs));
            }
            Location::Stack(stack) => {
                // save the data in the register to the stack
                asm.push(Inst::Sw(rs, stack.base, stack.offset));
            }
            Location::Data(data) => {
                let label = data.label.clone();
                asm.push(Inst::La(FREE_REG, label));
                asm.push(Inst::Sw(rs, FREE_REG, data.offset));
            }
        }
        Ok(())
    }

    pub fn load_imm_to(&mut self, imm: i32, dest: &mut RegPack) -> Result<(), AsmError> {
        // We never reuse immediate, only load it to the register.
        dest.insts.push(Inst::Li(dest.reg, imm));
        Ok(())
    }

    /// Build a reference to the location, and save the address to the register.
    pub fn load_ref_to(&mut self, stack: Stack, dest: &mut RegPack) {
        let reg = dest.reg;
        dest.refer = Some(stack.clone());
        dest.insts.push(Inst::Addi(reg, stack.base, stack.offset));
    }

    /// Load the data from the address of the src to the register.
    pub fn load_deref_to(&mut self, src: RegPack, dest: &mut RegPack) {
        dest.insts.push(Inst::Lw(dest.reg, src.reg, 0));
    }

    /// Save the data in the register to the address of the dest.
    pub fn save_deref_to(&mut self, src: RegPack, dest: RegPack, asm: &mut AsmProgram) {
        let rs1 = src.reg;
        let rs2 = dest.reg;
        self.write_pack(src, asm);
        match &dest.refer {
            Some(stack) => {
                asm.push(Inst::Sw(rs1, stack.base, stack.offset));
            }
            None => {
                self.write_pack(dest, asm);
                asm.push(Inst::Sw(rs1, rs2, 0));
            }
        }
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
                pack.refer = None;
                let mut p = RegPack::new(FREE_REG);
                self.load_val_to(ptr.clone(), &mut p)?;
                self.write_pack(p, asm);

                let mut p = RegPack::new(self.new_reg());
                self.load_imm_to(unit_size as i32, &mut p)?;
                let unit = p.reg;
                self.write_pack(p, asm);

                pack.insts.push(Inst::Mul(FREE_REG, FREE_REG, unit));
                pack.insts.push(Inst::Add(reg, reg, FREE_REG));
            }
            AsmElement::Imm(imm) => {
                let inc = imm * (unit_size as i32);
                if let Some(stack) = &mut pack.refer {
                    stack.offset += inc;
                }
                pack.insts.push(Inst::Addi(reg, reg, inc));
            }
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_val_to(&mut self, src: Pointer, dest: &mut RegPack) -> Result<(), AsmError> {
        let dest_reg = dest.reg;
        dest.refer = self.refer(src)?.clone();
        match self.loc(src)? {
            Location::Register(reg) => {
                if *reg != dest_reg {
                    dest.insts.push(Inst::Mv(dest_reg, *reg));
                    dest.reg = dest_reg;
                }
            }
            Location::Stack(stack) => {
                dest.insts
                    .push(Inst::Lw(dest_reg, stack.base, stack.offset));
                dest.reg = dest_reg;
            }
            Location::Data(data) => {
                dest.insts.push(Inst::La(dest_reg, data.label.clone()));
                dest.insts.push(Inst::Addi(dest_reg, dest_reg, data.offset));
                dest.reg = dest_reg;
            }
        }
        Ok(())
    }

    pub fn load_to(&mut self, element: &AsmElement, dest: &mut RegPack) -> Result<(), AsmError> {
        match element {
            AsmElement::Local(ptr) => self.load_val_to(*ptr, dest),
            AsmElement::Imm(imm) => self.load_imm_to(*imm, dest),
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
                let mut pack = RegPack::new(reg);
                self.load_to(&e, &mut pack)?;
                self.write_pack(pack, asm);
            }
            Location::Stack(stack) => {
                let mut pack = RegPack::new(FREE_REG);
                self.load_to(&e, &mut pack)?;
                self.write_pack(pack, asm);
                asm.push(Inst::Sw(FREE_REG, stack.base, stack.offset));
            }
            Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        }
        Ok(())
    }
}
