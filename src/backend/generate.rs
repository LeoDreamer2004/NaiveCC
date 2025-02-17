//! Main module for generating assembly code from IR. (Backend)

use super::assign::RegisterAssigner;
use super::env::{Environment, IntoElement};
use super::instruction::*;
use super::manager::WrapperPack;
use super::program::{AsmGlobal, AsmLocal, AsmProgram, Section};
use super::{opt_glb, registers, AsmResult, INT_SIZE};
use crate::utils::namer::original_ident;
use koopa::ir::entities::ValueData;
use koopa::ir::values::*;
use koopa::ir::{BinaryOp, FunctionData, Program, TypeKind, ValueKind};
use std::cell::Ref;

///////////////////////////////////////////
///         Entities Generator          ///
///////////////////////////////////////////

/// Trait for generating assembly code.
///
/// This trait is implemented by all the IR entities that can be translated into assembly code.
pub trait EntityAsmGenerator {
    /// The target type for the generated assembly code.
    type AsmTarget;

    /// Generate assembly code for the entity.
    ///
    /// # Errors
    /// AsmError is returned if the generation fails.
    fn generate_on(&self, env: &mut Environment, asm: &mut Self::AsmTarget) -> AsmResult<()>;
}

impl EntityAsmGenerator for Program {
    type AsmTarget = AsmProgram;

    fn generate_on(&self, env: &mut Environment, asm: &mut AsmProgram) -> AsmResult<()> {
        // global data
        for &g_value in self.inst_layout() {
            let g_data = self.borrow_value(g_value);
            let label = g_data.name().clone().unwrap()[1..].to_string();

            let mut glb = AsmGlobal::new(Section::Data, label.clone());
            let mut local = AsmLocal::new(None);

            if let ValueKind::GlobalAlloc(alloc) = g_data.kind() {
                let data = self.borrow_value(alloc.init());
                generate_global_data(data, env.ctx.program, &mut local);
            }
            glb.new_local(local);
            asm.new_global(glb);
            env.table.global_new(&*g_data, label);
        }

        // function data
        for &func in self.func_layout() {
            env.ctx.function = Some(func);
            let func_data = self.func(func);
            // skip declaration
            if func_data.layout().entry_bb().is_some() {
                let label = original_ident(&func_data.name().to_string());
                let mut glb = AsmGlobal::new(Section::Text, label);

                let label = format!(".prologue_{}", env.func_index);
                env.func_index += 1;

                let mut prologue = env.sf.build_prologue(func_data);
                prologue.label_mut().replace(label.clone());
                glb.new_local(prologue);

                func_data.generate_on(env, &mut glb)?;
                asm.new_global(glb);
            }
        }
        Ok(())
    }
}

fn generate_global_data(data: Ref<ValueData>, program: &Program, asm: &mut AsmLocal) {
    match data.kind() {
        ValueKind::ZeroInit(_) => {
            asm.insts_mut().push(Inst::Zero(data.ty().size()));
        }
        ValueKind::Integer(int) => {
            let value = int.value();
            let inst = if value == 0 {
                Inst::Zero(INT_SIZE)
            } else {
                Inst::Word(value)
            };
            asm.insts_mut().push(inst);
        }
        ValueKind::Aggregate(aggr) => {
            for &value in aggr.elems() {
                let data = program.borrow_value(value);
                generate_global_data(data, program, asm);
            }
        }
        _ => unreachable!(),
    }

    // Merge the last two zero if possible
    let insts = asm.insts_mut();
    if insts.len() >= 2 {
        let last = insts.pop().unwrap();
        let prev = insts.pop().unwrap();
        if let (Inst::Zero(a), Inst::Zero(b)) = (&prev, &last) {
            insts.push(Inst::Zero(a + b));
        } else {
            insts.push(prev);
            insts.push(last);
        }
    }
}

impl EntityAsmGenerator for FunctionData {
    type AsmTarget = AsmGlobal;

    fn generate_on(&self, env: &mut Environment, asm: &mut AsmGlobal) -> AsmResult<()> {
        // load params first
        let mut param = AsmLocal::new(Some(".params".into()));
        env.table.load_func_param(self, &mut param)?;
        asm.new_local(param);

        for (&bb, node) in self.layout().bbs() {
            let mut local = AsmLocal::new(Some(env.l_gen.get_name(bb)));
            for &inst in node.insts().keys() {
                self.dfg().value(inst).generate_on(env, &mut local)?;
            }
            asm.new_local(local);
        }

        opt_glb(asm);
        RegisterAssigner::default().assign(asm, &mut env.sf);
        env.table.end_func();
        env.sf.end_fill(asm)?;
        Ok(())
    }
}

impl EntityAsmGenerator for ValueData {
    type AsmTarget = AsmLocal;

    fn generate_on(&self, env: &mut Environment, asm: &mut AsmLocal) -> AsmResult<()> {
        match self.kind() {
            ValueKind::Alloc(alloc) => alloc.generate_on(self, env, asm),
            ValueKind::Store(store) => store.generate_on(self, env, asm),
            ValueKind::Load(load) => load.generate_on(self, env, asm),
            ValueKind::Binary(binary) => binary.generate_on(self, env, asm),
            ValueKind::Return(ret) => ret.generate_on(self, env, asm),
            ValueKind::Jump(jump) => jump.generate_on(self, env, asm),
            ValueKind::Branch(branch) => branch.generate_on(self, env, asm),
            ValueKind::Call(call) => call.generate_on(self, env, asm),
            ValueKind::GetElemPtr(elem_ptr) => elem_ptr.generate_on(self, env, asm),
            ValueKind::GetPtr(ptr) => ptr.generate_on(self, env, asm),
            _ => unreachable!("Valuekind cannot be an instruction: {:?}", self.kind()),
        }
    }
}

///////////////////////////////////////////
///          Value Generator            ///
///////////////////////////////////////////

/// Trait for generating values.
///
/// This trait is implemented by all the [`ValueKind`] that can be translated into assembly code.
trait ValueAsmGenerator {
    /// Generate assembly code for the value when known the return data.
    ///
    /// # Errors
    ///     
    /// AsmError is returned if the generation fails.
    fn generate_on(
        &self,
        ret_data: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()>;
}

impl ValueAsmGenerator for Alloc {
    fn generate_on(
        &self,
        ret_data: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let size = match ret_data.ty().kind() {
            TypeKind::Pointer(p) => p.size(),
            _ => unreachable!("Alloc type should always be a pointer"),
        };
        // malloc the data
        let stack = env.sf.malloc(size)?;
        // new value "self" as a pointer
        let reg = env.table.new_reg();
        let mut pack = WrapperPack::new(reg);
        env.table.load_ref_to(stack, &mut pack);
        env.table.new_val_with_src(ret_data, pack, asm)
    }
}

impl ValueAsmGenerator for Store {
    fn generate_on(
        &self,
        _: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let src = env.new_pack(self.value())?;
        let dest = env.new_pack(self.dest())?;
        env.table.save_to_deref(src, dest, asm);
        Ok(())
    }
}

impl ValueAsmGenerator for Load {
    fn generate_on(
        &self,
        ret_data: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let mut pack = env.new_pack(self.src())?;
        env.table.load_deref_to(pack.clone(), &mut pack);
        env.table.new_val_with_src(ret_data, pack, asm)
    }
}

impl ValueAsmGenerator for Binary {
    fn generate_on(
        &self,
        ret_data: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let p1 = env.new_pack(self.lhs())?;
        let p2 = env.new_pack(self.rhs())?;
        let rs1 = p1.reg;
        let rs2 = p2.reg;
        p1.write_on(asm);
        p2.write_on(asm);
        let rd = env.table.new_reg();
        let insts = match self.op() {
            BinaryOp::Add => vec![Inst::Add(rd, rs1, rs2)],
            BinaryOp::Sub => vec![Inst::Sub(rd, rs1, rs2)],
            BinaryOp::Mul => vec![Inst::Mul(rd, rs1, rs2)],
            BinaryOp::Div => vec![Inst::Div(rd, rs1, rs2)],
            BinaryOp::Mod => vec![Inst::Rem(rd, rs1, rs2)],
            BinaryOp::Lt => vec![Inst::Slt(rd, rs1, rs2)],
            BinaryOp::Gt => vec![Inst::Sgt(rd, rs1, rs2)],
            BinaryOp::And => vec![Inst::And(rd, rs1, rs2)],
            BinaryOp::Or => vec![Inst::Or(rd, rs1, rs2)],
            BinaryOp::Xor => vec![Inst::Xor(rd, rs1, rs2)],
            BinaryOp::Shl => vec![Inst::Sll(rd, rs1, rs2)],
            BinaryOp::Shr => vec![Inst::Srl(rd, rs1, rs2)],
            BinaryOp::Sar => vec![Inst::Sra(rd, rs1, rs2)],

            BinaryOp::Eq => vec![
                // a == b => (a ^ b) == 0
                Inst::Xor(rd, rs1, rs2),
                Inst::SeqZ(rd, rd),
            ],
            BinaryOp::NotEq => vec![
                // a != b => (a ^ b) != 0
                Inst::Xor(rd, rs1, rs2),
                Inst::SneZ(rd, rd),
            ],
            BinaryOp::Ge => vec![
                // a >= b => (a < b) == 0
                Inst::Slt(rd, rs1, rs2),
                Inst::SeqZ(rd, rd),
            ],
            BinaryOp::Le => vec![
                // a <= b => (a > b) == 0
                Inst::Sgt(rd, rs1, rs2),
                Inst::SeqZ(rd, rd),
            ],
        };
        asm.insts_mut().extend(insts);
        let pack = WrapperPack::new(rd);
        env.table.new_val_with_src(ret_data, pack, asm)
    }
}

impl ValueAsmGenerator for Return {
    fn generate_on(
        &self,
        _: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        if let Some(value) = self.value() {
            let e = value.into_elem(&env.ctx);
            let mut pack = WrapperPack::new(registers::A0);
            env.table.load_to(&e, &mut pack)?;
            pack.write_on(asm);
        }
        env.sf.build_epilogue(asm)?;
        asm.insts_mut().push(Inst::Ret);
        Ok(())
    }
}

impl ValueAsmGenerator for Jump {
    fn generate_on(
        &self,
        _: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let label = self.target();
        asm.insts_mut().push(Inst::J(env.l_gen.get_name(label)));
        Ok(())
    }
}

impl ValueAsmGenerator for Branch {
    fn generate_on(
        &self,
        _: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let pack = env.new_pack(self.cond())?;
        let rs = pack.reg;
        pack.write_on(asm);
        let true_bb = self.true_bb();
        asm.insts_mut()
            .push(Inst::Bnez(rs, env.l_gen.get_name(true_bb)));
        let false_bb = self.false_bb();
        asm.insts_mut().push(Inst::J(env.l_gen.get_name(false_bb)));
        Ok(())
    }
}

impl ValueAsmGenerator for Call {
    fn generate_on(
        &self,
        ret_data: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        for (index, &p) in self.args().iter().enumerate() {
            let param = env.ctx.to_ptr(p);
            env.table.save_func_param(index, param, asm)?;
        }
        let func_data = env.ctx.program.func(self.callee());
        let ident = original_ident(&func_data.name().to_string());
        asm.insts_mut().push(Inst::Call(ident));

        match func_data.ty().kind() {
            TypeKind::Function(_, ret) => {
                if !ret.is_unit() {
                    // return value
                    let reg = env.table.new_val(ret_data);
                    asm.insts_mut().push(Inst::Mv(reg, registers::A0));
                }
            }
            _ => unreachable!("Call callee should always be a function"),
        };

        Ok(())
    }
}

impl ValueAsmGenerator for GetElemPtr {
    fn generate_on(
        &self,
        ret_data: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let ty = env.ctx.value_type(self.src());
        let size = match ty.kind() {
            TypeKind::Pointer(p) => match p.kind() {
                TypeKind::Array(a, _) => a.size(),
                _ => unreachable!("GetElemPtr source pointer should be an array"),
            },
            _ => unreachable!("GetElemPtr source should always be a pointer"),
        };
        let mut pack = env.new_pack(self.src())?;
        let bias = self.index().into_elem(&env.ctx);
        env.table.add_bias(&mut pack, &bias, size)?;
        env.table.new_val_with_src(ret_data, pack, asm)
    }
}

impl ValueAsmGenerator for GetPtr {
    fn generate_on(
        &self,
        ret_data: &ValueData,
        env: &mut Environment,
        asm: &mut AsmLocal,
    ) -> AsmResult<()> {
        let ty = env.ctx.value_type(self.src());
        let size = match ty.kind() {
            TypeKind::Pointer(p) => p.size(),
            _ => unreachable!("GetElemPtr source should always be a pointer"),
        };
        let mut pack = env.new_pack(self.src())?;
        let bias = &self.index().into_elem(&env.ctx);
        env.table.add_bias(&mut pack, bias, size)?;
        env.table.new_val_with_src(ret_data, pack, asm)
    }
}
