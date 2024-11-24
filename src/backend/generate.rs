use super::assign::RegisterAssigner;
use super::env::{Environment, IntoElement};
use super::instruction::*;
use super::location::ToLocation;
use super::manager::RegPack;
use super::opt::AsmOptimizeManager;
use super::program::{AsmGlobal, AsmLocal, AsmProgram};
use super::registers;
use super::INT_SIZE;
use crate::utils::namer::original_ident;
use koopa::ir::entities::ValueData;
use koopa::ir::{BinaryOp, Function, FunctionData, Program, TypeKind, ValueKind};
use std::cell::Ref;
use std::io;

pub fn build_asm(program: Program) -> Result<AsmProgram, AsmError> {
    // generate
    let mut asm = AsmProgram::new();
    let mut env = Environment::new(&program, &mut asm);
    program.generate_on(&mut env)?;

    // optimization
    let mut optman = AsmOptimizeManager::default();
    asm = optman.run(asm);
    Ok(asm)
}

pub fn emit_asm(program: AsmProgram, output: impl io::Write) -> Result<(), io::Error> {
    program.emit(output)
}

#[derive(Debug)]
pub enum AsmError {
    NullLocation(Option<String>),
    IllegalGetAddress,
    FunctionNotFound(Function),
    InvalidStackFrame,
    StackOverflow,
}

/// Trait for generating assembly code.
///
/// This trait is implemented by all the IR entities that can be translated into assembly code.
pub trait GenerateAsm {
    /// Generate assembly code for the entity.
    ///
    /// # Errors
    /// AsmError is returned if the generation fails.
    fn generate_on(&self, env: &mut Environment) -> Result<(), AsmError>;
}

impl GenerateAsm for Program {
    fn generate_on(&self, env: &mut Environment) -> Result<(), AsmError> {
        for &g_value in self.inst_layout() {
            let g_data = self.borrow_value(g_value);
            let label = g_data.name().clone().unwrap()[1..].to_string();

            let mut glb = AsmGlobal::new(Directive::Data, label.clone());
            glb.new_local(AsmLocal::new(None));
            env.asm.new_global(glb);

            if let ValueKind::GlobalAlloc(alloc) = g_data.kind() {
                let data = self.borrow_value(alloc.init());
                generate_global_data(data, env.ctx.program, env.asm);
            }

            env.man.global_new(&*g_data, label);
        }

        for &func in self.func_layout() {
            env.ctx.function = Some(func);
            let func_data = self.func(func);
            // skip declaration
            if !func_data.layout().entry_bb().is_none() {
                func_data.generate_on(env)?;
            }
        }
        Ok(())
    }
}

fn generate_global_data(data: Ref<ValueData>, program: &Program, asm: &mut AsmProgram) {
    match data.kind() {
        ValueKind::ZeroInit(_) => {
            asm.push(Inst::Zero(data.ty().size()));
        }
        ValueKind::Integer(int) => {
            let value = int.value();
            let inst = if value == 0 {
                Inst::Zero(INT_SIZE)
            } else {
                Inst::Word(value)
            };
            asm.push(inst);
        }
        ValueKind::Aggregate(aggr) => {
            for &value in aggr.elems() {
                let data = program.borrow_value(value);
                generate_global_data(data, program, asm);
            }
        }
        _ => unreachable!(),
    }
}

impl GenerateAsm for FunctionData {
    fn generate_on(&self, env: &mut Environment) -> Result<(), AsmError> {
        env.man.new_func(self, env.asm);
        env.fs.prologue(self, env.asm);

        // load params first
        for (index, &p) in self.params().iter().enumerate() {
            let param = env.ctx.to_ptr(p);
            env.man.load_func_param(index, param)?;
        }

        for (&bb, node) in self.layout().bbs() {
            let local = AsmLocal::new(Some(env.label_gen.get_name(bb)));
            env.asm.cur_global_mut().new_local(local);
            for &inst in node.insts().keys() {
                self.dfg().value(inst).generate_on(env)?;
            }
        }

        let asm = env.asm.cur_global_mut();
        RegisterAssigner::default().assign(asm, &mut env.fs);

        env.man.end_func();
        env.fs.end(env.asm)?;
        Ok(())
    }
}

impl GenerateAsm for ValueData {
    fn generate_on(&self, env: &mut Environment) -> Result<(), AsmError> {
        match self.kind() {
            ValueKind::Integer(_) => {
                unreachable!("Integer is not an instruction in KoopaIR");
            }
            ValueKind::Alloc(_) => {
                let size = match self.ty().kind() {
                    TypeKind::Pointer(p) => p.size(),
                    _ => unreachable!("Alloc type should always be a pointer"),
                };
                // malloc the data
                let location = env.fs.malloc(size)?.to_loc();
                // new value "self" as a pointer
                let reg = env.man.new_reg();
                let mut pack = RegPack::new(reg);
                env.man.load_ref_to(location, &mut pack, env.asm)?;
                env.man.new_val_with_src(self, &mut pack, env.asm)?;
            }
            ValueKind::Store(store) => {
                let mut src = env.new_pack(store.value())?;
                let mut dest = env.new_pack(store.dest())?;
                env.man.save_deref_to(&mut src, &mut dest, env.asm);
            }
            ValueKind::Load(load) => {
                let mut pack = env.new_pack(load.src())?;
                env.man.load_deref_to(&pack.clone(), &mut pack, env.asm);
                env.man.new_val_with_src(self, &mut pack, env.asm)?;
            }
            ValueKind::Binary(binary) => {
                let rs1 = env.new_pack(binary.lhs())?.reg;
                let rs2 = env.new_pack(binary.rhs())?.reg;
                let rd = env.man.new_reg();
                let insts = match binary.op() {
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

                for inst in insts {
                    env.asm.push(inst);
                }

                let pack = &mut RegPack::new(rd);
                env.man.new_val_with_src(self, pack, env.asm)?;
            }
            ValueKind::Return(ret) => {
                if let Some(value) = ret.value() {
                    let e = value.into_element(&env.ctx);
                    let pack = &mut RegPack::new(registers::A0);
                    env.man.load_to(&e, pack, env.asm)?;
                }
                env.fs.epilogue(env.asm)?;
                env.asm.push(Inst::Ret);
            }
            ValueKind::Jump(jump) => {
                let label = jump.target();
                env.asm.push(Inst::J(env.label_gen.get_name(label)));
            }
            ValueKind::Branch(branch) => {
                let rs = env.new_pack(branch.cond())?.reg;
                let true_bb = branch.true_bb();
                env.asm
                    .push(Inst::Bnez(rs, env.label_gen.get_name(true_bb)));
                let false_bb = branch.false_bb();
                env.asm.push(Inst::J(env.label_gen.get_name(false_bb)));
            }
            ValueKind::Call(call) => {
                for (index, &p) in call.args().iter().enumerate() {
                    let param = env.ctx.to_ptr(p);
                    env.man.save_func_param(index, param, env.asm)?;
                }
                let func_data = env.ctx.program.func(call.callee());
                let ident = original_ident(&func_data.name().to_string());
                env.asm.push(Inst::Call(ident));
                let is_unit = match func_data.ty().kind() {
                    TypeKind::Function(_, ret) => ret.is_unit(),
                    _ => unreachable!("Call callee should always be a function"),
                };
                if !is_unit {
                    let reg = env.man.new_val(self, env.asm);
                    env.asm.push(Inst::Mv(reg, registers::A0));
                }
            }
            ValueKind::GetElemPtr(ptr) => {
                let value = env.ctx.func_data().dfg().values().get(&ptr.src());
                let ty = match value {
                    Some(v) => v.ty().clone(),
                    None => env.ctx.program.borrow_value(ptr.src()).ty().clone(),
                };
                let size = match ty.kind() {
                    TypeKind::Pointer(p) => match p.kind() {
                        TypeKind::Array(a, _) => a.size(),
                        _ => unreachable!("GetElemPtr source pointer should be an array"),
                    },
                    _ => unreachable!("GetElemPtr source should always be a pointer"),
                };
                let mut pack = env.new_pack(ptr.src())?;
                let bias = &ptr.index().into_element(&env.ctx);
                env.man.add_bias(&mut pack, bias, size, env.asm)?;
                env.man.new_val_with_src(self, &mut pack, env.asm)?;
            }
            ValueKind::GetPtr(ptr) => {
                let ty = env.ctx.value_type(ptr.src());
                let size = match ty.kind() {
                    TypeKind::Pointer(p) => p.size(),
                    _ => unreachable!("GetElemPtr source should always be a pointer"),
                };
                let mut pack = env.new_pack(ptr.src())?;
                let bias = &ptr.index().into_element(&env.ctx);
                env.man.add_bias(&mut pack, bias, size, env.asm)?;
                env.man.new_val_with_src(self, &mut pack, env.asm)?;
            }
            _ => unimplemented!(),
        }
        Ok(())
    }
}
