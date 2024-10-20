use super::instruction::*;
use super::register::{self, RegisterDispatcher, RegisterType, RiscVRegister};
use crate::common::NameGenerator;
use koopa::ir::entities::ValueData;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Value, ValueKind};
use std::io::Write;

pub trait GenerateAsm<T> {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<T, AsmError>;
}

pub struct Context<'a> {
    pub dispatcher: RegisterDispatcher,
    pub label_gen: NameGenerator<BasicBlock>,

    pub program: &'a Program,
    pub function: Option<Function>,
    pub value: Option<Value>,
}

impl<'a> Context<'a> {
    pub fn new(program: &'a Program) -> Self {
        Context {
            dispatcher: RegisterDispatcher::default(),
            label_gen: NameGenerator::new(|id| format!(".l{}", id)),

            program,
            function: None,
            value: None,
        }
    }
}

macro_rules! func_data {
    ($context:expr) => {
        $context.program.func($context.function.unwrap())
    };
}

macro_rules! value_data {
    ($context:expr, $value:expr) => {
        func_data!($context).dfg().value($value)
    };
}

macro_rules! original_ident {
    ($func_data:expr) => {
        $func_data.name()[1..].to_string()
    };
}

#[derive(Debug)]
pub enum AsmError {
    RegisterNotAssigned(Option<String>),
    FunctionNotFound(Function),
    InvalidStackFrame,
}

impl GenerateAsm<()> for Program {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
        asm.push(Inst::Directive(Directive::Text));
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

impl GenerateAsm<()> for FunctionData {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
        asm.push(Inst::Directive(Directive::Globl(original_ident!(self))));
        asm.push(Inst::Label(original_ident!(self)));
        context.dispatcher.new_frame(asm);
        for (&bb, node) in self.layout().bbs() {
            asm.push(Inst::Label(context.label_gen.get_name(bb)));
            for &inst in node.insts().keys() {
                context.value = Some(inst);
                self.dfg().value(inst).generate_on(context, asm)?;
            }
        }
        context.dispatcher.end_frame(asm)?;
        // dbg!(&context.dispatcher);
        Ok(())
    }
}

impl GenerateAsm<()> for ValueData {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
        match self.kind() {
            ValueKind::Integer(_) => {
                unreachable!("Integer must be generated by ElementData")
            }
            ValueKind::Alloc(_) => {
                // TODO: Malloc or Register? That's a question
                context.dispatcher.malloc(self, 4)
            }
            ValueKind::Store(store) => {
                let rs = store.value().into_element().generate_on(context, asm)?;
                let dest = value_data!(context, store.dest());
                context.dispatcher.save(dest, rs, asm)?;
                Ok(())
            }
            ValueKind::Load(load) => {
                let rs = load.src().into_element().generate_on(context, asm)?;
                context.dispatcher.new(self)?;
                context.dispatcher.save(self, rs, asm)?;
                Ok(())
            }
            ValueKind::Binary(binary) => {
                let rs1 = binary.lhs().into_element().generate_on(context, asm)?;
                let rs2 = binary.rhs().into_element().generate_on(context, asm)?;
                let rd = context.dispatcher.dispatch(RegisterType::Local);
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

                context.dispatcher.new(self)?;
                context.dispatcher.save(self, rd, asm)?;

                context.dispatcher.release(rs1);
                context.dispatcher.release(rs2);
                Ok(())
            }
            ValueKind::Return(ret) => {
                if let Some(value) = ret.value() {
                    let rs = value.into_element().generate_on(context, asm)?;
                    asm.push(Inst::Mv(Mv(&register::A0, rs)));
                    asm.push(Inst::Ret(Ret {}));
                    context.dispatcher.release(rs);
                }
                Ok(())
            }
            ValueKind::Jump(jump) => {
                let label = jump.target();
                // func_data!(context).dfg().bbs().get(&label).unwrap();
                asm.push(Inst::J(J(context.label_gen.get_name(label))));
                Ok(())
            }
            ValueKind::Branch(branch) => {
                let rs = branch.cond().into_element().generate_on(context, asm)?;
                let true_bb = branch.true_bb();
                asm.push(Inst::Bnez(Bnez(rs, context.label_gen.get_name(true_bb))));
                let false_bb = branch.false_bb();
                asm.push(Inst::J(J(context.label_gen.get_name(false_bb))));
                context.dispatcher.release(rs);
                Ok(())
            }
            // ValueKind::Call(call) => {
            // todo!()
            // }
            _ => todo!(),
        }
    }
}

/// Basic element in an instruction, including immediate value and register.
pub struct ElementData {
    pub value: Value,
}

pub trait IntoElement {
    fn into_element(self) -> ElementData;
}

impl IntoElement for Value {
    fn into_element(self) -> ElementData {
        ElementData {
            value: self.clone(),
        }
    }
}

impl GenerateAsm<RiscVRegister> for ElementData {
    fn generate_on(
        &self,
        context: &mut Context,
        asm: &mut AsmProgram,
    ) -> Result<RiscVRegister, AsmError> {
        let data = value_data!(context, self.value);
        match data.kind() {
            ValueKind::Integer(int) => {
                let imm = int.value();
                if imm == 0 {
                    // simple optimization for zero
                    return Ok(&register::ZERO);
                };
                let rd = context.dispatcher.dispatch(RegisterType::Local);
                asm.push(Inst::Li(Li(rd, imm)));
                Ok(rd)
            }
            _ => context.dispatcher.load_or_error(data, asm),
        }
    }
}

pub fn build_asm(ir_program: Program) -> Result<AsmProgram, AsmError> {
    let mut asm = AsmProgram::new();
    let mut context = Context::new(&ir_program);
    ir_program.generate_on(&mut context, &mut asm)?;
    Ok(asm)
}

pub fn emit_asm(program: AsmProgram, mut output: impl Write) -> Result<(), std::io::Error> {
    for inst in program {
        writeln!(output, "{}", inst.dump())?;
    }
    Ok(())
}
