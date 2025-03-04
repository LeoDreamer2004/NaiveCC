//! Environment for generating IR

use super::loops::LoopStack;
use super::symbol::SymbolTable;
use koopa::ir::builder::{GlobalBuilder, LocalBuilder};
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, Value, ValueKind};

/*********************  Utils  *********************/

/// Create a new [`BasicBlock`] in [`Function`]
#[macro_export]
macro_rules! new_bb {
    ($func:expr) => {
        $func.dfg_mut().new_bb()
    };
}

/// Add a [`BasicBlock`] to the layout of [`Function`]
#[macro_export]
macro_rules! add_bb {
    ($func:expr, $bb:expr) => {
        $func.layout_mut().bbs_mut().push_key_back($bb).unwrap()
    };
}

/// Get all instructions in a [`BasicBlock`] in [`Function`]
#[macro_export]
macro_rules! all_insts {
    ($func:expr, $bb:expr) => {
        $func.layout_mut().bb_mut($bb).insts_mut()
    };
}

/// Add an instruction to a [`BasicBlock`] in [`Function`]
macro_rules! add_inst {
    ($func:expr, $bb:expr, $inst:expr) => {
        all_insts!($func, $bb).push_key_back($inst).unwrap()
    };
}

/// Context for current generating
#[derive(Default)]
pub struct Context {
    pub program: Program,
    pub func: Option<Function>,
    pub block: Option<BasicBlock>,
}

/// All environment data
#[derive(Default)]
pub struct Environment {
    pub table: SymbolTable,
    pub ctx: Context,
    pub loops: LoopStack,
}

pub enum ContextBuilder<'a> {
    Global(GlobalBuilder<'a>),
    Local(LocalBuilder<'a>),
}

impl<'a> ContextBuilder<'a> {
    pub fn integer(self, value: i32) -> Value {
        match self {
            ContextBuilder::Global(builder) => builder.integer(value),
            ContextBuilder::Local(builder) => builder.integer(value),
        }
    }

    pub fn aggregate(self, values: Vec<Value>) -> Value {
        match self {
            ContextBuilder::Global(builder) => builder.aggregate(values),
            ContextBuilder::Local(builder) => builder.aggregate(values),
        }
    }

    pub fn zero_init(self, ty: Type) -> Value {
        match self {
            ContextBuilder::Global(builder) => builder.zero_init(ty),
            ContextBuilder::Local(builder) => builder.zero_init(ty),
        }
    }
}

impl Context {
    pub fn func(&mut self, func: Function) {
        self.func = Some(func);
    }

    pub fn block(&mut self, block: BasicBlock) {
        self.block = Some(block);
    }

    pub fn func_data(&mut self) -> &mut FunctionData {
        self.program.func_mut(self.func.unwrap())
    }

    pub fn local_val(&mut self) -> LocalBuilder {
        self.func_data().dfg_mut().new_value()
    }

    pub fn glb_val(&mut self) -> GlobalBuilder {
        self.program.new_value()
    }

    pub fn val(&mut self) -> ContextBuilder {
        if self.is_global() {
            ContextBuilder::Global(self.glb_val())
        } else {
            ContextBuilder::Local(self.local_val())
        }
    }

    pub fn set_value_name(&mut self, value: Value, name: String) {
        if self.is_global() {
            self.program.set_value_name(value, Some(name));
        } else {
            self.func_data().dfg_mut().set_value_name(value, Some(name));
        }
    }

    pub fn is_global(&self) -> bool {
        self.func.is_none()
    }

    /// Create a new block and set it as the current block
    pub fn create_block(&mut self, name: Option<String>) {
        let block = new_bb!(self.func_data()).basic_block(name);
        add_bb!(self.func_data(), block);
        self.block(block);
    }

    pub fn if_block_ended(&mut self, bb: &BasicBlock) -> bool {
        let last = all_insts!(self.func_data(), *bb).back_key().copied();
        match last {
            Some(inst) => {
                let kind = self.func_data().dfg().value(inst).kind();
                match kind {
                    ValueKind::Jump(_) => true,
                    ValueKind::Branch(_) => true,
                    ValueKind::Return(_) => true,
                    _ => false,
                }
            }
            None => false,
        }
    }

    /// Generate a new block and append it to the layout
    /// If enabled `auto_link`, It will automatically add a jump to the next block if needed
    pub fn new_block(&mut self, name: Option<String>, auto_link: bool) -> BasicBlock {
        let current_block_ended = self.if_block_ended(&self.block.unwrap());
        let block = self.block.unwrap();
        let func_data = self.func_data();
        if !all_insts!(func_data, block).is_empty() || !auto_link {
            // If the last block is not empty, or forced not to link, create a new block
            let this = new_bb!(func_data).basic_block(name);
            add_bb!(func_data, this);

            // If the current block is not ended, add a jump to the next block
            if auto_link && !current_block_ended {
                let jump = self.local_val().jump(this);
                self.add_inst(jump);
            }
            self.block(this);
        }
        self.block.unwrap()
    }

    pub fn alloc_and_store(&mut self, value: Value, ty: Type) -> Value {
        let alloc = self.local_val().alloc(ty);
        let store = self.local_val().store(value, alloc);
        self.add_inst(alloc);
        self.add_inst(store);
        alloc
    }

    pub fn in_entry(&mut self) -> bool {
        self.func_data().layout().bbs().len() == 1
    }

    pub fn add_inst(&mut self, inst: Value) {
        let block = self.block.unwrap();
        add_inst!(self.func_data(), block, inst);
    }

    /// Get the type of the value.
    pub fn value_type(&self, value: Value) -> Type {
        let v = self
            .program
            .func(self.func.unwrap())
            .dfg()
            .values()
            .get(&value);
        match v {
            Some(v) => v.ty().clone(),
            None => self.program.borrow_value(value).ty().clone(),
        }
    }
}
