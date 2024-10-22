use super::instruction::*;
use super::register::{
    self, AsmElement, RegisterDispatcher, RegisterType, RiscVRegister, INT_SIZE,
};
use crate::common::NameGenerator;
use koopa::ir::entities::ValueData;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Value, ValueKind};
use std::io::Write;

macro_rules! func_data {
    ($context:expr) => {
        $context.program.func($context.function.unwrap())
    };
}

macro_rules! value_data {
    ($context:expr, $value:expr) => {
        // if $value.is_global() {
            // &*($context.program.borrow_value($value))
        // } else {
            func_data!($context).dfg().value($value)
        // }
    };
}

macro_rules! original_ident {
    ($func_data:expr) => {
        $func_data.name()[1..].to_string()
    };
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

    pub fn load_value_to_reg(
        &mut self,
        value: Value,
        asm: &mut AsmProgram,
    ) -> Result<RiscVRegister, AsmError> {
        let e = value.into_element(self);
        self.dispatcher.load(&e, asm)
    }
}

#[derive(Debug)]
pub enum AsmError {
    NullLocation(Option<String>),
    FunctionNotFound(Function),
    InvalidStackFrame,
}

pub trait GenerateAsm<T> {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<T, AsmError>;
}

impl GenerateAsm<()> for Program {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
        for &g_values in self.inst_layout() {
            asm.push(Inst::Directive(Directive::Data));
            let g_data = value_data!(context, g_values);
            let label = g_data.name().clone().unwrap()[1..].to_string();
            asm.push(Inst::Directive(Directive::Globl(label.clone())));
            asm.push(Inst::Label(label.clone()));

            if let ValueKind::GlobalAlloc(alloc) = g_data.kind() {
                let data = value_data!(context, alloc.init());
                match data.kind() {
                    ValueKind::ZeroInit(_) => {
                        asm.push(Inst::Directive(Directive::Zero(INT_SIZE)));
                    }
                    ValueKind::Integer(int) => {
                        asm.push(Inst::Directive(Directive::Word(int.value())));
                    }
                    _ => todo!(),
                }
            }

            context.dispatcher.global_new(g_data, label);
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

impl GenerateAsm<()> for FunctionData {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
        asm.push(Inst::Directive(Directive::Text));
        let label = original_ident!(self);
        asm.push(Inst::Directive(Directive::Globl(label.clone())));
        asm.push(Inst::Label(label));
        context.dispatcher.new_frame(self, asm);

        // load params first
        for (index, &p) in self.params().iter().enumerate() {
            let param = value_data!(context, p);
            context.dispatcher.load_func_param(index, param, asm)?;
        }

        for (&bb, node) in self.layout().bbs() {
            asm.push(Inst::Label(context.label_gen.get_name(bb)));
            for &inst in node.insts().keys() {
                context.value = Some(inst);
                self.dfg().value(inst).generate_on(context, asm)?;
            }
        }
        context.dispatcher.end_frame(asm)?;
        Ok(())
    }
}

impl GenerateAsm<()> for ValueData {
    fn generate_on(&self, context: &mut Context, asm: &mut AsmProgram) -> Result<(), AsmError> {
        match self.kind() {
            ValueKind::Integer(_) => {
                unreachable!("Integer must be generated as Immediate");
            }
            ValueKind::Alloc(_) => {
                // TODO: Ensure the size of the allocated memory
                context.dispatcher.malloc(self, INT_SIZE)?;
            }
            ValueKind::Store(store) => {
                let rs = context.load_value_to_reg(store.value(), asm)?;
                let dest = value_data!(context, store.dest());
                context.dispatcher.save_val_to(dest, rs, asm)?;
            }
            ValueKind::Load(load) => {
                let rs = context.load_value_to_reg(load.src(), asm)?;
                context.dispatcher.new_val(self)?;
                context.dispatcher.save_val_to(self, rs, asm)?;
            }
            ValueKind::Binary(binary) => {
                let rs1 = context.load_value_to_reg(binary.lhs(), asm)?;
                let rs2 = context.load_value_to_reg(binary.rhs(), asm)?;
                let rd = context.dispatcher.dispatch(RegisterType::Temp);
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

                context.dispatcher.new_val(self)?;
                context.dispatcher.save_val_to(self, rd, asm)?;

                context.dispatcher.release(rs1);
                context.dispatcher.release(rs2);
            }
            ValueKind::Return(ret) => {
                if let Some(value) = ret.value() {
                    let e = value.into_element(context);
                    context.dispatcher.load_to(&e, register::A0, asm)?;
                }
                context.dispatcher.out_frame(asm)?;
                asm.push(Inst::Ret(Ret {}));
            }
            ValueKind::Jump(jump) => {
                let label = jump.target();
                // context.dfg().bbs().get(&label).unwrap();
                asm.push(Inst::J(J(context.label_gen.get_name(label))));
            }
            ValueKind::Branch(branch) => {
                let rs = context.load_value_to_reg(branch.cond(), asm)?;
                let true_bb = branch.true_bb();
                asm.push(Inst::Bnez(Bnez(rs, context.label_gen.get_name(true_bb))));
                let false_bb = branch.false_bb();
                asm.push(Inst::J(J(context.label_gen.get_name(false_bb))));
                context.dispatcher.release(rs);
            }
            ValueKind::Call(call) => {
                for (index, &p) in call.args().iter().enumerate() {
                    let param = value_data!(context, p);
                    context.dispatcher.save_func_param(index, param, asm)?;
                }
                let func_data = context.program.func(call.callee());
                let ident = original_ident!(func_data);
                asm.push(Inst::Call(Call(ident)));
                context.dispatcher.new_val(self)?;

                // FIXME
                if !func_data.ty().is_unit() {
                    context.dispatcher.save_val_to(self, register::A0, asm)?;
                }
            }
            _ => todo!(),
        }
        Ok(())
    }
}

trait IntoElement {
    fn into_element(self, context: &Context) -> AsmElement;
}

impl IntoElement for Value {
    fn into_element(self, context: &Context) -> AsmElement {
        let data = value_data!(context, self);
        AsmElement::from(data)
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
