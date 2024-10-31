use super::instruction::*;
use super::manager::{AsmElement, AsmManager, Pointer, RegPack};
use super::opt::*;
use super::register::{self, RegisterType, ANY_REG};
use super::INT_SIZE;
use crate::utils::namer::{original_ident, NameGenerator};
use koopa::ir::entities::ValueData;
use koopa::ir::{
    BasicBlock, BinaryOp, Function, FunctionData, Program, Type, TypeKind, Value, ValueKind,
};
use std::cell::Ref;
use std::io::Write;

pub struct Context<'a> {
    pub manager: AsmManager,
    pub label_gen: NameGenerator<BasicBlock>,

    pub program: &'a Program,
    pub function: Option<Function>,
    pub value: Option<Value>,
}

impl<'a> Context<'a> {
    pub fn new(program: &'a Program) -> Self {
        Context {
            manager: AsmManager::default(),
            label_gen: NameGenerator::new(|id| format!(".l{}", id)),

            program,
            function: None,
            value: None,
        }
    }

    pub fn func_data(&self) -> &FunctionData {
        self.program.func(self.function.unwrap())
    }

    pub fn local_data(&self, value: Value) -> &ValueData {
        self.func_data().dfg().value(value)
    }

    pub fn global_data(&self, value: Value) -> Ref<ValueData> {
        self.program.borrow_value(value)
    }

    pub fn value_type(&self, value: Value) -> Type {
        let v = self.func_data().dfg().values().get(&value);
        match v {
            Some(v) => v.ty().clone(),
            None => self.global_data(value).ty().clone(),
        }
    }

    pub fn to_ptr(&self, value: Value) -> Pointer {
        if value.is_global() {
            &*self.global_data(value)
        } else {
            self.local_data(value)
        }
    }

    pub fn new_pack(&mut self, value: Value, asm: &mut AsmProgram) -> Result<RegPack, AsmError> {
        let e = value.into_element(self);
        let mut pack = RegPack::new(ANY_REG);
        self.manager.load_to(&e, &mut pack, asm)?;
        Ok(pack)
    }
}

#[derive(Debug)]
pub enum AsmError {
    NullLocation(Option<String>),
    IllegalGetAddress,
    FunctionNotFound(Function),
    InvalidStackFrame,
    StackOverflow,
}

pub trait GenerateAsm {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError>;
}

impl GenerateAsm for Program {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
        for &g_value in self.inst_layout() {
            asm.push(Inst::Directive(Directive::Data));
            let g_data = self.borrow_value(g_value);
            let label = g_data.name().clone().unwrap()[1..].to_string();
            asm.push(Inst::Directive(Directive::Globl(label.clone())));
            asm.push(Inst::Label(label.clone()));

            if let ValueKind::GlobalAlloc(alloc) = g_data.kind() {
                let data = self.borrow_value(alloc.init());
                generate_global_data(data, context.program, asm);
            }

            context.manager.global_new(&*g_data, label);
        }

        for &func in self.func_layout() {
            context.function = Some(func);
            let func_data = self.func(func);
            // skip declaration
            if !func_data.layout().entry_bb().is_none() {
                func_data.generate_on(context, asm)?;
            }
        }
        Ok(())
    }
}

fn generate_global_data(data: Ref<ValueData>, program: &Program, asm: &mut AsmProgram) {
    match data.kind() {
        ValueKind::ZeroInit(_) => {
            asm.push(Inst::Directive(Directive::Zero(data.ty().size())));
        }
        ValueKind::Integer(int) => {
            let value = int.value();
            let directive = if value == 0 {
                Directive::Zero(INT_SIZE)
            } else {
                Directive::Word(value)
            };
            asm.push(Inst::Directive(directive));
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
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
        asm.push(Inst::Directive(Directive::Text));
        let label = original_ident(&self.name().to_string());
        asm.push(Inst::Directive(Directive::Globl(label.clone())));
        asm.push(Inst::Label(label));
        context.manager.fh().new_frame(self, asm);

        // load params first
        for (index, &p) in self.params().iter().enumerate() {
            let param = context.to_ptr(p);
            context.manager.load_func_param(index, param)?;
        }

        for (&bb, node) in self.layout().bbs() {
            asm.push(Inst::Label(context.label_gen.get_name(bb)));
            for &inst in node.insts().keys() {
                context.value = Some(inst);
                self.dfg().value(inst).generate_on(context, asm)?;
            }
        }
        context.manager.fh().end_frame(asm)?;
        Ok(())
    }
}

impl GenerateAsm for ValueData {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
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
                let location = context.manager.malloc(size)?;
                // new value "self" as a pointer
                let reg = context.manager.dpt().dispatch(RegisterType::Temp);
                let mut pack = RegPack::new(reg);
                context.manager.load_ref_to(location, &mut pack, asm)?;
                context.manager.new_val(self)?;
                context.manager.save_val_to(self, &mut pack, asm)?;
                context.manager.dpt().release(reg);
            }
            ValueKind::Store(store) => {
                let mut src = context.new_pack(store.value(), asm)?;
                let mut dest = context.new_pack(store.dest(), asm)?;
                context.manager.save_deref_to(&mut src, &mut dest, asm);
                context.manager.dpt().release(src.reg);
                context.manager.dpt().release(dest.reg);
            }
            ValueKind::Load(load) => {
                let mut pack = context.new_pack(load.src(), asm)?;
                context.manager.load_deref_to(&pack.clone(), &mut pack, asm);
                context.manager.new_val(self)?;
                context.manager.save_val_to(self, &mut pack, asm)?;
            }
            ValueKind::Binary(binary) => {
                let rs1 = context.new_pack(binary.lhs(), asm)?.reg;
                let rs2 = context.new_pack(binary.rhs(), asm)?.reg;
                let rd = context.manager.dpt().dispatch(RegisterType::Temp);
                let insts = match binary.op() {
                    BinaryOp::Add => vec![Inst::Add(Add(rd, rs1, rs2))],
                    BinaryOp::Sub => vec![Inst::Sub(Sub(rd, rs1, rs2))],
                    BinaryOp::Mul => vec![Inst::Mul(Mul(rd, rs1, rs2))],
                    BinaryOp::Div => vec![Inst::Div(Div(rd, rs1, rs2))],
                    BinaryOp::Mod => vec![Inst::Rem(Rem(rd, rs1, rs2))],
                    BinaryOp::Lt => vec![Inst::Slt(Slt(rd, rs1, rs2))],
                    BinaryOp::Gt => vec![Inst::Sgt(Sgt(rd, rs1, rs2))],
                    BinaryOp::And => vec![Inst::And(And(rd, rs1, rs2))],
                    BinaryOp::Or => vec![Inst::Or(Or(rd, rs1, rs2))],
                    BinaryOp::Xor => vec![Inst::Xor(Xor(rd, rs1, rs2))],
                    BinaryOp::Shl => vec![Inst::Sll(Sll(rd, rs1, rs2))],
                    BinaryOp::Shr => vec![Inst::Srl(Srl(rd, rs1, rs2))],
                    BinaryOp::Sar => vec![Inst::Sra(Sra(rd, rs1, rs2))],

                    BinaryOp::Eq => vec![
                        // a == b => (a ^ b) == 0
                        Inst::Xor(Xor(rd, rs1, rs2)),
                        Inst::SeqZ(SeqZ(rd, rd)),
                    ],
                    BinaryOp::NotEq => vec![
                        // a != b => (a ^ b) != 0
                        Inst::Xor(Xor(rd, rs1, rs2)),
                        Inst::SneZ(SneZ(rd, rd)),
                    ],
                    BinaryOp::Ge => vec![
                        // a >= b => (a < b) == 0
                        Inst::Slt(Slt(rd, rs1, rs2)),
                        Inst::SeqZ(SeqZ(rd, rd)),
                    ],
                    BinaryOp::Le => vec![
                        // a <= b => (a > b) == 0
                        Inst::Sgt(Sgt(rd, rs1, rs2)),
                        Inst::SeqZ(SeqZ(rd, rd)),
                    ],
                };
                asm.extend(insts);

                context.manager.new_val(self)?;
                context
                    .manager
                    .save_val_to(self, &mut RegPack::new(rd), asm)?;

                context.manager.dpt().release(rs1);
                context.manager.dpt().release(rs2);
            }
            ValueKind::Return(ret) => {
                if let Some(value) = ret.value() {
                    let e = value.into_element(context);
                    context
                        .manager
                        .load_to(&e, &mut RegPack::new(register::A0), asm)?;
                }
                context.manager.fh().out_frame(asm)?;
                asm.push(Inst::Ret(Ret));
            }
            ValueKind::Jump(jump) => {
                let label = jump.target();
                asm.push(Inst::J(J(context.label_gen.get_name(label))));
            }
            ValueKind::Branch(branch) => {
                let rs = context.new_pack(branch.cond(), asm)?.reg;
                let true_bb = branch.true_bb();
                asm.push(Inst::Bnez(Bnez(rs, context.label_gen.get_name(true_bb))));
                let false_bb = branch.false_bb();
                asm.push(Inst::J(J(context.label_gen.get_name(false_bb))));
                context.manager.dpt().release(rs);
            }
            ValueKind::Call(call) => {
                for (index, &p) in call.args().iter().enumerate() {
                    let param = context.to_ptr(p);
                    context.manager.save_func_param(index, param, asm)?;
                }
                let func_data = context.program.func(call.callee());
                let ident = original_ident(&func_data.name().to_string());
                asm.push(Inst::Call(Call(ident)));
                context.manager.new_val(self)?;

                let is_unit = match func_data.ty().kind() {
                    TypeKind::Function(_, ret) => ret.is_unit(),
                    _ => unreachable!("Call callee should always be a function"),
                };
                if !is_unit {
                    let mut pack = RegPack::new(register::A0);
                    context.manager.save_val_to(self, &mut pack, asm)?;
                }
            }
            ValueKind::GetElemPtr(ptr) => {
                let value = context.func_data().dfg().values().get(&ptr.src());
                let ty = match value {
                    Some(v) => v.ty().clone(),
                    None => context.program.borrow_value(ptr.src()).ty().clone(),
                };
                let size = match ty.kind() {
                    TypeKind::Pointer(p) => match p.kind() {
                        TypeKind::Array(a, _) => a.size(),
                        _ => unreachable!("GetElemPtr source pointer should be an array"),
                    },
                    _ => unreachable!("GetElemPtr source should always be a pointer"),
                };
                let mut pack = context.new_pack(ptr.src(), asm)?;
                let bias = &ptr.index().into_element(context);
                context.manager.add_bias(&mut pack, bias, size, asm)?;
                context.manager.new_val(self)?;
                context.manager.save_val_to(self, &mut pack, asm)?;
            }
            ValueKind::GetPtr(ptr) => {
                let ty = context.value_type(ptr.src());
                let size = match ty.kind() {
                    TypeKind::Pointer(p) => p.size(),
                    _ => unreachable!("GetElemPtr source should always be a pointer"),
                };
                let mut pack = context.new_pack(ptr.src(), asm)?;
                let bias = &ptr.index().into_element(context);
                context.manager.add_bias(&mut pack, bias, size, asm)?;
                context.manager.new_val(self)?;
                context.manager.save_val_to(self, &mut pack, asm)?;
                // context.manager.mark_as_ptr(self);
            }
            _ => unimplemented!(),
        }
        Ok(())
    }
}

trait IntoElement {
    fn into_element(self, context: &Context) -> AsmElement;
}

impl IntoElement for Value {
    fn into_element(self, context: &Context) -> AsmElement {
        let data = context.to_ptr(self);
        AsmElement::from(data)
    }
}

pub fn build_asm(ir_program: Program) -> Result<AsmProgram, AsmError> {
    let mut asm = AsmProgram::new();
    let mut context = Context::new(&ir_program);
    ir_program.generate_on(&mut context, &mut asm)?;

    let mut optman = AsmOptimizeManager::new();
    optman.add(Box::new(ImmFixOptimizer::new()));
    optman.add(Box::new(AlgorithmOptimizer::new()));
    asm = optman.run(asm);
    Ok(asm)
}

pub fn emit_asm(program: AsmProgram, mut output: impl Write) -> Result<(), std::io::Error> {
    for inst in program {
        writeln!(output, "{}", inst.dump())?;
    }
    Ok(())
}
