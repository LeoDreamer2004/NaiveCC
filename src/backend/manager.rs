use super::address::{Data, Descriptor, Stack};
use super::program::AsmLocal;
use super::registers::*;
use super::{instruction::*, AsmResult};
use super::{AsmError, INT_SIZE, MAX_PARAM_REG};
use koopa::ir::entities::ValueData;
use koopa::ir::{FunctionData, ValueKind};
use std::collections::HashMap;

/// Pointer to the data, also the key of the [`DescriptorTable`].
pub type Pointer = *const ValueData;

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

#[derive(Debug, Clone)]
struct DataStatus {
    /// where the data is stored
    descriptor: Descriptor,
    /// The reference what the data stored
    refer: Option<Stack>,
}

impl DataStatus {
    fn new(desc: Descriptor) -> Self {
        Self {
            descriptor: desc,
            refer: None,
        }
    }
}

#[derive(Debug, Default)]
pub struct DescriptorTable {
    map: HashMap<Pointer, DataStatus>,
    glb_map: HashMap<Pointer, DataStatus>,
    reg_index: usize,
}

/// A pack of information, including the register,
/// (optional) address reference, and cached instructions.
#[derive(Debug, Clone)]
pub struct WrapperPack {
    pub reg: Register,
    refer: Option<Stack>,
    insts: Vec<Inst>,
}

impl WrapperPack {
    pub fn new(reg: Register) -> Self {
        Self {
            reg,
            refer: None,
            insts: Vec::new(),
        }
    }

    /// Write the cached instructions to the given [`AsmLocal`].
    pub fn write_on(self, asm: &mut AsmLocal) {
        asm.insts_mut().extend(self.insts);
    }

    /// Merge the cached instructions to another pack.
    pub fn merge_into(self, other: &mut WrapperPack) {
        other.insts.extend(self.insts);
    }
}

impl DescriptorTable {
    pub fn add_desc(&mut self, ptr: Pointer, desc: Descriptor) {
        self.map.insert(ptr, DataStatus::new(desc));
    }

    pub fn add_glb_desc(&mut self, ptr: Pointer, desc: Descriptor) {
        self.glb_map.insert(ptr, DataStatus::new(desc));
    }

    pub fn desc(&self, ptr: Pointer) -> AsmResult<&Descriptor> {
        self.status(ptr).map(|status| &status.descriptor)
    }

    pub fn refer(&self, ptr: Pointer) -> AsmResult<&Option<Stack>> {
        self.status(ptr).map(|status| &status.refer)
    }

    pub fn refer_mut(&mut self, ptr: Pointer) -> AsmResult<&mut Option<Stack>> {
        self.status_mut(ptr).map(|status| &mut status.refer)
    }

    fn status(&self, ptr: Pointer) -> AsmResult<&DataStatus> {
        self.map
            .get(&ptr)
            .ok_or(AsmError::NullLocation(format!("{ptr:?}")))
    }

    fn status_mut(&mut self, ptr: Pointer) -> AsmResult<&mut DataStatus> {
        self.map
            .get_mut(&ptr)
            .ok_or(AsmError::NullLocation(format!("{ptr:?}")))
    }

    pub fn end_func(&mut self) {
        self.map = self.glb_map.clone();
    }

    pub fn global_new(&mut self, ptr: Pointer, label: Label) {
        let desc: Descriptor = Data { label, offset: 0 }.into();
        self.add_desc(ptr, desc.clone());
        self.add_glb_desc(ptr, desc);
    }

    pub fn new_reg(&mut self) -> Register {
        self.reg_index += 1;
        Register::Fake(FakeRegister(self.reg_index - 1))
    }

    pub fn new_val(&mut self, ptr: Pointer) -> Register {
        let reg = self.new_reg();
        self.add_desc(ptr, reg.into());
        reg
    }

    pub fn new_val_with_src(
        &mut self,
        ptr: Pointer,
        src: WrapperPack,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let address = src.reg.into();
        self.add_desc(ptr, address);
        self.save_val_to(ptr, src, asm)
    }

    /// Save data in the register to the address of the dest.
    pub fn save_val_to(
        &mut self,
        dest: Pointer,
        src: WrapperPack,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let rs = src.reg;
        *self.refer_mut(dest)? = src.refer.clone();
        src.write_on(asm);
        let insts = asm.insts_mut();

        match self.desc(dest)? {
            Descriptor::Register(reg) => {
                insts.push(Inst::Mv(*reg, rs));
            }
            Descriptor::Stack(stack) => {
                // save the data in the register to the stack
                insts.push(Inst::Sw(rs, stack.base, stack.offset));
            }
            Descriptor::Data(data) => {
                let label = data.label.clone();
                insts.push(Inst::La(FREE_REG, label));
                insts.push(Inst::Sw(rs, FREE_REG, data.offset));
            }
        }
        Ok(())
    }

    /// Load the data from the address of the src to the register.
    pub fn load_val_to(&mut self, src: Pointer, dest: &mut WrapperPack) -> AsmResult<()> {
        let dest_reg = dest.reg;
        dest.refer = self.refer(src)?.clone();
        match self.desc(src)? {
            Descriptor::Register(reg) => {
                dest.insts.push(Inst::Mv(dest_reg, *reg));
                dest.reg = dest_reg;
            }
            Descriptor::Stack(stack) => {
                dest.insts
                    .push(Inst::Lw(dest_reg, stack.base, stack.offset));
                dest.reg = dest_reg;
            }
            Descriptor::Data(data) => {
                dest.insts.push(Inst::La(dest_reg, data.label.clone()));
                dest.insts.push(Inst::Addi(dest_reg, dest_reg, data.offset));
                dest.reg = dest_reg;
            }
        }
        Ok(())
    }

    pub fn load_imm_to(&mut self, imm: i32, dest: &mut WrapperPack) -> AsmResult<()> {
        // We never reuse immediate, only load it to the register.
        // Do those later in optimization.
        dest.insts.push(Inst::Li(dest.reg, imm));
        Ok(())
    }

    pub fn load_to(&mut self, element: &AsmElement, dest: &mut WrapperPack) -> AsmResult<()> {
        match element {
            AsmElement::Local(ptr) => self.load_val_to(*ptr, dest),
            AsmElement::Imm(imm) => self.load_imm_to(*imm, dest),
        }
    }

    /// Build a reference to the stack, and save the address to the register.
    pub fn load_ref_to(&mut self, stack: Stack, dest: &mut WrapperPack) {
        let reg = dest.reg;
        dest.refer = Some(stack.clone());
        dest.insts.push(Inst::Addi(reg, stack.base, stack.offset));
    }

    /// Load the data from the address of the src to the register.
    pub fn load_deref_to(&mut self, src: WrapperPack, dest: &mut WrapperPack) {
        if let Some(stack) = dest.refer.clone() {
            for (_, status) in &self.map {
                if status.refer == dest.refer {
                    dest.insts = vec![Inst::Lw(dest.reg, stack.base, stack.offset)];
                    return;
                }
            }
        }
        dest.insts.push(Inst::Lw(dest.reg, src.reg, 0));
    }

    /// Save the data in the register to the address of the dest.
    pub fn save_to_deref(&mut self, src: WrapperPack, dest: WrapperPack, asm: &mut AsmLocal) {
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
        pack: &mut WrapperPack,
        bias: &AsmElement,
        unit_size: usize,
    ) -> AsmResult<()> {
        let reg = pack.reg;
        match bias {
            AsmElement::Local(ptr) => {
                pack.refer = None;
                let mut p = WrapperPack::new(FREE_REG);
                self.load_val_to(ptr.clone(), &mut p)?;
                p.merge_into(pack);
                let mut p = WrapperPack::new(self.new_reg());
                self.load_imm_to(unit_size as i32, &mut p)?;
                let unit = p.reg;
                p.merge_into(pack);
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

    fn param_desc(index: usize) -> Descriptor {
        if index < MAX_PARAM_REG {
            RegisterType::Arg.all()[index].into()
        } else {
            let offset = ((index - MAX_PARAM_REG) * INT_SIZE) as i32;
            Stack { base: SP, offset }.into()
        }
    }

    pub fn load_func_param(
        &mut self,
        func_data: &FunctionData,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let mut max_args = 0;
        for (_, data) in func_data.dfg().values() {
            if let ValueKind::Call(call) = data.kind() {
                max_args = max_args.max(call.args().len());
            }
        }

        for (index, &p) in func_data.params().iter().enumerate() {
            let descriptor = match Self::param_desc(index) {
                Descriptor::Register(reg) => {
                    if index >= max_args {
                        reg.into()
                    } else {
                        // may be covered, use a new register
                        let real = self.new_reg();
                        let inst = Inst::Mv(real, reg);
                        asm.insts_mut().push(inst);
                        real.into()
                    }
                }
                Descriptor::Stack(stack) => {
                    let offset = stack.offset;
                    Stack { base: FP, offset }.into()
                }
                Descriptor::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
            };
            let param = func_data.dfg().value(p);
            self.add_desc(param, descriptor);
        }

        Ok(())
    }

    pub fn save_func_param(
        &mut self,
        index: usize,
        param: Pointer,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let e = AsmElement::from(param);
        match Self::param_desc(index) {
            Descriptor::Register(reg) => {
                let mut pack = WrapperPack::new(reg);
                self.load_to(&e, &mut pack)?;
                pack.write_on(asm);
            }
            Descriptor::Stack(stack) => {
                let mut pack = WrapperPack::new(FREE_REG);
                self.load_to(&e, &mut pack)?;
                pack.write_on(asm);
                asm.insts_mut()
                    .push(Inst::Sw(FREE_REG, stack.base, stack.offset));
            }
            Descriptor::Data(_) => unreachable!("Function parameter cannot be saved in .data"),
        }
        Ok(())
    }
}
