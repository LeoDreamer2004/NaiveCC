use koopa::ir::{FunctionData, ValueKind};

use super::instruction::*;
use super::location::{AsmElement, Data, Location, Pointer, Stack, ToLocation};
use super::program::AsmLocal;
use super::registers::*;
use super::{AsmError, INT_SIZE, MAX_PARAM_REG};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct PointerInfo {
    /// where the data is stored
    location: Location,
    /// The reference what the data stored
    refer: Option<Stack>,
}

#[derive(Debug, Default)]
pub struct AsmManager {
    map: HashMap<Pointer, PointerInfo>,
    glb_map: HashMap<Pointer, PointerInfo>,
    reg_index: usize,
    func_index: usize,
}

#[derive(Debug, Clone)]
pub struct InfoPack {
    pub reg: Register,
    refer: Option<Stack>,
    insts: Vec<Inst>,
}

impl InfoPack {
    pub fn new(reg: Register) -> Self {
        Self {
            reg,
            refer: None,
            insts: Vec::new(),
        }
    }

    pub fn write_on(self, asm: &mut AsmLocal) {
        asm.insts_mut().extend(self.insts);
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

    pub fn new_func_idx(&mut self) -> usize {
        self.func_index += 1;
        self.func_index
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

    pub fn new_val(&mut self, ptr: Pointer) -> Register {
        let reg = self.new_reg();
        self.add_loc(ptr, reg.to_loc());
        reg
    }

    pub fn new_val_with_src(
        &mut self,
        ptr: Pointer,
        src: InfoPack,
        asm: &mut AsmLocal,
    ) -> Result<(), AsmError> {
        let address = src.reg.to_loc();
        self.add_loc(ptr, address);
        self.save_val_to(ptr, src, asm)
    }

    /// Save data in the register to the address of the dest.
    pub fn save_val_to(
        &mut self,
        dest: Pointer,
        src: InfoPack,
        asm: &mut AsmLocal,
    ) -> Result<(), AsmError> {
        let rs = src.reg;
        *self.refer_mut(dest)? = src.refer.clone();
        src.write_on(asm);
        let insts = asm.insts_mut();

        match self.loc(dest)? {
            Location::Register(reg) => {
                if *reg == rs {
                    return Ok(());
                }
                insts.push(Inst::Mv(*reg, rs));
            }
            Location::Stack(stack) => {
                // save the data in the register to the stack
                insts.push(Inst::Sw(rs, stack.base, stack.offset));
            }
            Location::Data(data) => {
                let label = data.label.clone();
                insts.push(Inst::La(FREE_REG, label));
                insts.push(Inst::Sw(rs, FREE_REG, data.offset));
            }
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_val_to(&mut self, src: Pointer, dest: &mut InfoPack) -> Result<(), AsmError> {
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

    pub fn load_imm_to(&mut self, imm: i32, dest: &mut InfoPack) -> Result<(), AsmError> {
        // We never reuse immediate, only load it to the register.
        // Do those later in optimization.
        dest.insts.push(Inst::Li(dest.reg, imm));
        Ok(())
    }

    pub fn load_to(&mut self, element: &AsmElement, dest: &mut InfoPack) -> Result<(), AsmError> {
        match element {
            AsmElement::Local(ptr) => self.load_val_to(*ptr, dest),
            AsmElement::Imm(imm) => self.load_imm_to(*imm, dest),
        }
    }

    /// Build a reference to the stack, and save the address to the register.
    pub fn load_ref_to(&mut self, stack: Stack, dest: &mut InfoPack) {
        let reg = dest.reg;
        dest.refer = Some(stack.clone());
        dest.insts.push(Inst::Addi(reg, stack.base, stack.offset));
    }

    /// Load the data from the address of the src to the register.
    pub fn load_deref_to(&mut self, src: InfoPack, dest: &mut InfoPack) {
        dest.insts.push(Inst::Lw(dest.reg, src.reg, 0));
    }

    /// Save the data in the register to the address of the dest.
    pub fn save_to_deref(&mut self, src: InfoPack, dest: InfoPack, asm: &mut AsmLocal) {
        let rs1 = src.reg;
        let rs2 = dest.reg;
        src.write_on(asm);
        match &dest.refer {
            Some(stack) => {
                asm.insts_mut()
                    .push(Inst::Sw(rs1, stack.base, stack.offset));
            }
            None => {
                dest.write_on(asm);
                asm.insts_mut().push(Inst::Sw(rs1, rs2, 0));
            }
        }
    }

    pub fn add_bias(
        &mut self,
        pack: &mut InfoPack,
        bias: &AsmElement,
        unit_size: usize,
        asm: &mut AsmLocal,
    ) -> Result<(), AsmError> {
        let reg = pack.reg;
        match bias {
            AsmElement::Local(ptr) => {
                pack.refer = None;
                let mut p = InfoPack::new(FREE_REG);
                self.load_val_to(ptr.clone(), &mut p)?;
                p.write_on(asm);

                if unit_size.count_ones() == 1 {
                    // Simple case: unit_size is a power of 2
                    pack.insts.push(Inst::Slli(
                        FREE_REG,
                        FREE_REG,
                        unit_size.trailing_zeros() as i32,
                    ));
                } else {
                    let mut p = InfoPack::new(self.new_reg());
                    self.load_imm_to(unit_size as i32, &mut p)?;
                    let unit = p.reg;
                    p.write_on(asm);
                    pack.insts.push(Inst::Mul(FREE_REG, FREE_REG, unit));
                }

                pack.insts.push(Inst::Add(reg, reg, FREE_REG));
            }
            AsmElement::Imm(imm) => {
                let inc = imm * (unit_size as i32);
                // if let Some(stack) = &mut pack.refer {
                //     stack.offset += inc;
                // }
                pack.refer = None;
                pack.insts.push(Inst::Addi(reg, reg, inc));
            }
        }
        Ok(())
    }

    fn param_location(index: usize) -> Location {
        if index < MAX_PARAM_REG {
            RegisterType::Arg.all()[index].to_loc()
        } else {
            let offset = ((index - MAX_PARAM_REG) * INT_SIZE) as i32;
            Stack { base: SP, offset }.to_loc()
        }
    }

    pub fn load_func_param(
        &mut self,
        func_data: &FunctionData,
        asm: &mut AsmLocal,
    ) -> Result<(), AsmError> {
        let mut max_args = 0;
        for (_, data) in func_data.dfg().values() {
            if let ValueKind::Call(call) = data.kind() {
                max_args = max_args.max(call.args().len());
            }
        }

        for (index, &p) in func_data.params().iter().enumerate() {
            let location = match Self::param_location(index) {
                Location::Register(reg) => {
                    if index >= max_args {
                        reg.to_loc()
                    } else {
                        let real = self.new_reg();
                        let inst = Inst::Mv(real, reg);
                        asm.insts_mut().push(inst);
                        real.to_loc()
                    }
                }
                Location::Stack(stack) => {
                    // the params are in the stack of the caller
                    let offset = stack.offset;
                    Stack { base: FP, offset }.to_loc()
                }
                Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
            };
            let param = func_data.dfg().value(p);
            self.add_loc(param, location);
        }

        Ok(())
    }

    pub fn save_func_param(
        &mut self,
        index: usize,
        param: Pointer,
        asm: &mut AsmLocal,
    ) -> Result<(), AsmError> {
        let e = AsmElement::from(param);
        match Self::param_location(index) {
            Location::Register(reg) => {
                let mut pack = InfoPack::new(reg);
                self.load_to(&e, &mut pack)?;
                pack.write_on(asm);
            }
            Location::Stack(stack) => {
                let mut pack = InfoPack::new(FREE_REG);
                self.load_to(&e, &mut pack)?;
                pack.write_on(asm);
                asm.insts_mut()
                    .push(Inst::Sw(FREE_REG, stack.base, stack.offset));
            }
            Location::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        }
        Ok(())
    }
}
