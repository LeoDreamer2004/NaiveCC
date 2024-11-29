//! Backend environment.

use super::frames::StackFrame;
use super::location::{AsmElement, Pointer};
use super::manager::{AsmManager, InfoPack};
use super::AsmError;
use crate::utils::namer::IdGenerator;
use koopa::ir::entities::ValueData;
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, Value};
use std::cell::Ref;

pub struct Context<'a> {
    pub program: &'a Program,
    pub function: Option<Function>,
}

impl<'a> Context<'a> {
    pub fn new(program: &'a Program) -> Self {
        Context {
            program,
            function: None,
        }
    }

    /// Get the current function data.
    pub fn func_data(&self) -> &FunctionData {
        self.program.func(self.function.unwrap())
    }

    /// Get the data of the local value.
    pub fn local_data(&self, value: Value) -> &ValueData {
        self.func_data().dfg().value(value)
    }

    /// Get the data of the global value.
    pub fn global_data(&self, value: Value) -> Ref<ValueData> {
        self.program.borrow_value(value)
    }

    /// Get the type of the value.
    pub fn value_type(&self, value: Value) -> Type {
        let v = self.func_data().dfg().values().get(&value);
        match v {
            Some(v) => v.ty().clone(),
            None => self.global_data(value).ty().clone(),
        }
    }

    /// Convert the value to a pointer.
    pub fn to_ptr(&self, value: Value) -> Pointer {
        if value.is_global() {
            &*self.global_data(value)
        } else {
            self.local_data(value)
        }
    }
}

pub struct Environment<'a> {
    pub ctx: Context<'a>,
    pub man: AsmManager,
    pub sf: StackFrame,
    pub l_gen: IdGenerator<BasicBlock>,
}

impl<'a> Environment<'a> {
    pub fn new(program: &'a Program) -> Self {
        Environment {
            ctx: Context::new(program),
            man: AsmManager::default(),
            sf: StackFrame::default(),
            l_gen: IdGenerator::new(|e| format!("L{}", e)),
        }
    }

    pub fn new_pack(&mut self, value: Value) -> Result<InfoPack, AsmError> {
        let e = value.into_element(&self.ctx);
        let mut pack = InfoPack::new(self.man.new_reg());
        self.man.load_to(&e, &mut pack)?;
        Ok(pack)
    }
}

pub trait IntoElement {
    fn into_element(self, ctx: &Context) -> AsmElement;
}

impl IntoElement for Value {
    fn into_element(self, ctx: &Context) -> AsmElement {
        let data = ctx.to_ptr(self);
        AsmElement::from(data)
    }
}
