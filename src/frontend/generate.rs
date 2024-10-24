//! Generate IR from AST

use super::ast::*;
use super::builtin::builtin_functions;
use super::eval::Eval;
use super::loops::*;
use super::symbol::*;
use koopa::back::KoopaGenerator;
use koopa::ir::builder::LocalBuilder;
use koopa::ir::builder_traits::*;
use koopa::ir::Type;
use koopa::ir::ValueKind;
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Value};
use std::io;

pub fn build_ir(ast: CompUnit) -> Result<Program, AstError> {
    let mut context = Context::default();
    ast.generate_on(&mut context)?;
    Ok(context.program)
}

pub fn emit_ir(program: &mut Program, output: impl io::Write) -> Result<(), io::Error> {
    KoopaGenerator::new(output).generate_on(program)
}

/*********************  Structs  *********************/

/// Context for current generating
#[derive(Default)]
pub struct Context {
    pub program: Program,
    pub syb_table: SymbolTable,
    pub func: Option<Function>,
    pub block: Option<BasicBlock>,
    pub loop_stack: LoopStack,
}

#[derive(Debug)]
pub enum AstError {
    FunctionNotFoundError(String),
    IllegalConstExpError(String),
    SymbolNotFoundError(String),
    InitializeError(String),
    IllegalAccessError(String),
    LoopStackError(String),
    AssignError(String),
    TypeError(String),
    UnknownError(String),
}

// /*********************  Traits  *********************/
/// Trait which should be implemented by AST types to generate IR
///
/// Return a value of type `T` if needed
trait GenerateIr<T> {
    /// Generate IR on the given context
    ///
    /// # Errors
    /// Returns an [`ParseError`] if the generation fails
    fn generate_on(&self, context: &mut Context) -> Result<T, AstError>;
}

/*********************  Utils  *********************/

/// Create a new [`BasicBlock`] in [`Function`]
macro_rules! new_bb {
    ($func:expr) => {
        $func.dfg_mut().new_bb()
    };
}

/// Create a new [`Value`] in [`Function`]
macro_rules! new_value {
    ($func:expr) => {
        $func.dfg_mut().new_value()
    };
}

/// Add a [`BasicBlock`] to the layout of [`Function`]
macro_rules! add_bb {
    ($func:expr, $bb:expr) => {
        $func.layout_mut().bbs_mut().push_key_back($bb).unwrap()
    };
}

/// Get all instructions in a [`BasicBlock`] in [`Function`]
macro_rules! all_insts {
    ($func:expr, $bb:expr) => {
        $func.layout_mut().bb_mut($bb).insts_mut()
    };
}

/// Add an [`Value`] to a [`BasicBlock`] in [`Function`]
macro_rules! add_inst {
    ($func:expr, $bb:expr, $inst:expr) => {
        all_insts!($func, $bb).push_key_back($inst).unwrap()
    };
}

/// Set the name of a [`Value`] in the [`FunctionData`]
macro_rules! set_value_name {
    ($func:expr, $value:expr, $name:expr) => {
        $func.dfg_mut().set_value_name($value, Some($name))
    };
}

/// Generate a global identifier in Koopa IR
macro_rules! global_ident {
    ($def:expr) => {
        format!("@{}", &$def.ident)
    };
}

/// Generate a normal identifier in Koopa IR
macro_rules! normal_ident {
    ($def:expr) => {
        format!("%{}", $def.ident)
    };
}

/*********************  Implementations  *********************/

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

    pub fn new_value(&mut self) -> LocalBuilder {
        self.func_data().dfg_mut().new_value()
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
                let jump = new_value!(func_data).jump(this);
                add_inst!(func_data, block, jump);
            }
            self.block(this);
        }
        self.block.unwrap()
    }

    pub fn alloc_and_store(&mut self, value: Value, b_type: Type) -> Value {
        let alloc = new_value!(self.func_data()).alloc(b_type);
        let store = new_value!(self.func_data()).store(value, alloc);
        self.add_inst(alloc);
        self.add_inst(store);
        alloc
    }

    pub fn in_entry(&mut self) -> bool {
        self.func_data().layout().bbs().len() == 1
    }

    pub fn add_inst(&mut self, inst: Value) {
        let func_data = self.program.func_mut(self.func.unwrap());
        let block = self.block.unwrap();
        add_inst!(func_data, block, inst);
    }
}

impl GenerateIr<()> for CompUnit {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        // builtin functions
        for func_data in builtin_functions() {
            context.program.new_func(func_data);
        }

        for comp_item in &self.comp_items {
            match comp_item {
                CompItem::FuncDef(func_def) => {
                    func_def.generate_on(context)?;
                }
                CompItem::Decl(decl) => {
                    decl.generate_on(context)?;
                }
            }
        }
        Ok(())
    }
}

impl GenerateIr<()> for FuncDef {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let func = FunctionData::with_param_names(
            global_ident!(self),
            self.params
                .iter()
                .map(|param| (Some(global_ident!(param)), param.b_type.into()))
                .collect(),
            self.func_type.into(),
        );

        let func = context.program.new_func(func);
        context.func(func);
        context.create_block(Some("%entry".into()));

        context.syb_table.enter_scope();

        // Add parameters to the symbol table
        for (i, param) in (&self.params).into_iter().enumerate() {
            context.syb_table.new_symbol(param)?;
            let value = context.func_data().params()[i];
            let alloc = context.alloc_and_store(value, param.b_type.into());
            set_value_name!(context.func_data(), alloc, normal_ident!(param));
            context.syb_table.set_alloc(&param.get_ident(), alloc)?;
        }

        self.block.generate_on(context)?;
        context.syb_table.exit_scope();

        let func_data = context.func_data();
        let ret = new_value!(func_data).ret(None);
        context.add_inst(ret);
        context.block = None;
        context.func = None;
        Ok(())
    }
}

impl GenerateIr<()> for Decl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.generate_on(context),
            Decl::VarDecl(var_decl) => var_decl.generate_on(context),
        }
    }
}

impl GenerateIr<()> for Block {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        if self.block_items.is_empty() {
            return Ok(());
        }

        context.new_block(None, true);
        context.syb_table.enter_scope();

        for block_item in &self.block_items {
            match block_item {
                BlockItem::Stmt(stmt) => {
                    stmt.generate_on(context)?;
                    match stmt {
                        Stmt::Return(_) => {
                            break;
                        }
                        Stmt::Break(_) => {
                            break;
                        }
                        Stmt::Continue(_) => {
                            break;
                        }
                        _ => {}
                    }
                }
                BlockItem::Decl(decl) => decl.generate_on(context)?,
            };
        }
        context.syb_table.exit_scope();
        context.new_block(None, true);
        Ok(())
    }
}

impl GenerateIr<()> for Stmt {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        match self {
            Stmt::Empty => Ok(()),
            Stmt::Exp(exp) => exp.generate_on(context).map(|_| ()),
            Stmt::Block(stmt) => stmt.generate_on(context),
            Stmt::Assign(stmt) => stmt.generate_on(context),
            Stmt::If(stmt) => stmt.generate_on(context),
            Stmt::While(stmt) => stmt.generate_on(context),
            Stmt::Return(stmt) => stmt.generate_on(context),
            Stmt::Break(stmt) => stmt.generate_on(context),
            Stmt::Continue(stmt) => stmt.generate_on(context),
        }
    }
}

impl GenerateIr<()> for Return {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let ret = match &self.exp {
            Some(exp) => Some(exp.generate_on(context)?),
            None => None,
        };

        let func_data = context.func_data();
        let ret = new_value!(func_data).ret(ret);
        context.add_inst(ret);
        Ok(())
    }
}

impl GenerateIr<()> for Assign {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let alloc = self.l_val.generate_on(context)?;
        let exp = self.exp.generate_on(context)?;

        // Store the value to the variable
        let func_data = context.func_data();
        let store = new_value!(func_data).store(exp, alloc);
        context.add_inst(store);
        Ok(())
    }
}

impl GenerateIr<()> for If {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let cond = self.cond.generate_on(context)?;
        let base_bb = context.block.unwrap();

        let true_bb = context.new_block(Some("%then".into()), false);
        self.stmt.generate_on(context)?;
        // always create a new ending block for the then statement
        // even if it is not a block statement
        let true_end_bb = context.new_block(Some("%then_end".into()), true);

        if let Some(else_stmt) = &self.else_stmt {
            let false_bb = context.new_block(Some("%else".into()), false);
            else_stmt.generate_on(context)?;
            let end_bb = context.new_block(None, true);

            let func_data = context.func_data();
            // branch to true/false
            let branch = new_value!(func_data).branch(cond, true_bb, false_bb);
            add_inst!(func_data, base_bb, branch);
            // true jump to end
            let jump = new_value!(func_data).jump(end_bb);
            add_inst!(func_data, true_end_bb, jump);
        } else {
            let end_bb = context.new_block(None, true);
            // branch to true/end
            let func_data = context.func_data();
            let branch = new_value!(func_data).branch(cond, true_bb, end_bb);
            add_inst!(func_data, base_bb, branch);
        };

        Ok(())
    }
}

impl GenerateIr<()> for While {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let block = context.block.unwrap();
        let cond_bb = if context.in_entry() {
            // The entry basic block is not allowed to have predecessors
            // so create a new block for the condition
            let cond_bb = context.new_block(Some("%cond".into()), false);
            let func_data = context.func_data();
            let jump = new_value!(func_data).jump(cond_bb);
            add_inst!(func_data, block, jump);
            cond_bb
        } else {
            context.new_block(Some("%cond".into()), true)
        };

        let cond = self.cond.generate_on(context)?;
        // use a temporary end block to serve for "break"
        let temp_end_bb = context.new_block(None, false);

        context.loop_stack.push(Loop {
            cond_bb,
            end_bb: temp_end_bb,
        });
        let body_bb = context.new_block(Some("%body".into()), false);
        self.stmt.generate_on(context)?;

        // jump back to the condition
        if !context.if_block_ended(&context.block.unwrap()) {
            let func_data = context.func_data();
            let jump = new_value!(func_data).jump(cond_bb);
            context.add_inst(jump);
        }

        let end_bb = context.new_block(None, true);
        let func_data = context.func_data();

        // branch to body/end
        let branch = new_value!(func_data).branch(cond, body_bb, end_bb);
        add_inst!(func_data, cond_bb, branch);

        // temp end block
        let jump = new_value!(func_data).jump(end_bb);
        add_inst!(func_data, temp_end_bb, jump);

        context.loop_stack.pop();
        Ok(())
    }
}

impl GenerateIr<()> for Break {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let end_bb = context
            .loop_stack
            .last()
            .ok_or(AstError::LoopStackError("break".into()))?
            .end_bb;
        let jump = new_value!(context.func_data()).jump(end_bb);
        context.add_inst(jump);
        Ok(())
    }
}

impl GenerateIr<()> for Continue {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let cond_bb = context
            .loop_stack
            .last()
            .ok_or(AstError::LoopStackError("continue".into()))?
            .cond_bb;
        let jump = new_value!(context.func_data()).jump(cond_bb);
        context.add_inst(jump);
        Ok(())
    }
}

impl GenerateIr<()> for ConstDecl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        for const_def in self.const_defs.clone() {
            context.syb_table.new_symbol(&const_def)?;
            let symbol = context.syb_table.lookup(&const_def.ident).unwrap();
            if symbol.is_single() {
                continue;
            }
            let size = symbol.size()?;
            let parse = const_def.const_init_val.parse(symbol.get_bias()?)?;

            if context.is_global() {
                let map = |exp: &ConstExp| exp.eval(&context.syb_table).unwrap();
                let values = parse
                    .unfold(map, 0)
                    .iter()
                    .map(|int| context.program.new_value().integer(*int))
                    .collect();
                let aggr = context.program.new_value().aggregate(values);
                context.program.new_value().global_alloc(aggr);
            } else {
                let alloc = new_value!(context.func_data())
                    .alloc(Type::get_array(self.b_type.into(), size));
                context.add_inst(alloc);

                for (exp, idx) in parse.filter() {
                    let eval = exp.eval(&context.syb_table)?;
                    let func_data = context.func_data();
                    let value = new_value!(func_data).integer(eval);
                    let index = new_value!(func_data).integer(idx as i32);
                    let ptr = new_value!(func_data).get_elem_ptr(alloc, index);
                    let store = new_value!(func_data).store(value, ptr);
                    context.add_inst(ptr);
                    context.add_inst(store);
                }

                context.syb_table.set_alloc(const_def.get_ident(), alloc)?;
            }
        }
        Ok(())
    }
}

impl GenerateIr<()> for VarDecl {
    fn generate_on(&self, context: &mut Context) -> Result<(), AstError> {
        let is_global = context.is_global();

        for var_def in self.var_defs.clone() {
            context.syb_table.new_symbol(&var_def)?;
            let symbol = context.syb_table.lookup(var_def.get_ident()).unwrap();
            let size = symbol.size();
            let is_single = symbol.is_single();

            if is_global {
                // GLOBAL VARIABLE

                // Allocate global memory for the variable
                let init = if is_single {
                    match &var_def.init_val {
                        Some(init) => {
                            let int = init.as_element()?.eval(&context.syb_table)?;
                            context.program.new_value().integer(int)
                        }
                        None => context.program.new_value().zero_init(self.b_type.into()),
                    }
                } else {
                    match &var_def.init_val {
                        Some(init) => {
                            let map = |exp: &Exp| exp.eval(&context.syb_table).unwrap();
                            let values = init
                                .parse(symbol.get_bias()?)?
                                .unfold(map, 0)
                                .iter()
                                .map(|int| context.program.new_value().integer(*int))
                                .collect();
                            context.program.new_value().aggregate(values)
                        }
                        None => context
                            .program
                            .new_value()
                            .zero_init(Type::get_array(self.b_type.into(), symbol.size()?)),
                    }
                };
                let alloc = context.program.new_value().global_alloc(init);
                let name = global_ident!(var_def);
                context.program.set_value_name(alloc, Some(name));
                context.syb_table.set_alloc(&var_def.ident, alloc)?;
            } else {
                // LOCAL VARIABLE

                let func_data = context.func_data();
                // Allocate memory for the variable
                let alloc = if is_single {
                    new_value!(func_data).alloc(self.b_type.into())
                } else {
                    new_value!(func_data).alloc(Type::get_array(self.b_type.into(), size?))
                };
                set_value_name!(func_data, alloc, normal_ident!(var_def));
                context.add_inst(alloc);
                
                if let Some(init) = &var_def.init_val {
                    if is_single {
                        let init = init.as_element()?.generate_on(context)?;
                        let store = new_value!(context.func_data()).store(init, alloc);
                        context.add_inst(store);
                    } else {
                        let symbol = context.syb_table.lookup(var_def.get_ident()).unwrap();
                        for (exp, idx) in init.parse(symbol.get_bias()?)?.filter() {
                            let value = exp.generate_on(context)?;
                            let func_data = context.func_data();
                            let index = new_value!(func_data).integer(idx as i32);
                            let ptr = new_value!(func_data).get_elem_ptr(alloc, index);
                            let store = new_value!(func_data).store(value, ptr);
                            context.add_inst(ptr);
                            context.add_inst(store);
                        }
                    };
                }
                context.syb_table.set_alloc(var_def.get_ident(), alloc)?;
            }
        }
        Ok(())
    }
}

impl GenerateIr<Value> for LVal {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match context.syb_table.lookup_item(&self.ident) {
            Some(item) => match &item {
                SymbolItem::Var(symbol) => symbol.get_alloc(),
                SymbolItem::VarArray(symbol) => {
                    let symbol = symbol.clone();
                    let alloc = symbol.get_alloc()?;
                    let mut indexes = vec![];
                    for exp in self.array_index.clone() {
                        indexes.push(exp.generate_on(context)?);
                    }
                    let bias = symbol.index(&indexes, context)?;
                    let addr = new_value!(context.func_data()).get_elem_ptr(alloc, bias);
                    context.add_inst(addr);
                    Ok(addr)
                },
                _ => unreachable!("Constants are not allowed to be assigned!"),
            },
            None => Err(AstError::SymbolNotFoundError(self.ident.clone())),
        }
    }
}

impl GenerateIr<Value> for Exp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        if let Ok(value) = self.eval(&context.syb_table) {
            return value.generate_on(context);
        }

        match self {
            Exp::LOrExp(l_or_exp) => l_or_exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for ConstExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        // Const expressions must be evaluated at compile time
        self.eval(&context.syb_table)?.generate_on(context)
    }
}

impl GenerateIr<Value> for Cond {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            Cond::LOrExp(exp) => exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for LOrExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.generate_on(context),
            LOrExp::LOrOpExp(op_exp) => {
                // short circuit

                // base block
                let func_data = context.func_data();
                let zero = new_value!(func_data).integer(0);
                let lhs = op_exp.l_or_exp.generate_on(context)?;
                let func_data = context.func_data();
                let l_binary = new_value!(func_data).binary(BinaryOp::NotEq, lhs, zero);
                context.add_inst(l_binary);
                let result = context.alloc_and_store(l_binary, Type::get_i32());
                let base_bb = context.block.unwrap();

                // false block
                let false_bb = context.new_block(Some("%or_else".into()), false);
                let rhs = op_exp.l_and_exp.generate_on(context)?;
                let func_data = context.func_data();
                let r_binary = new_value!(func_data).binary(BinaryOp::NotEq, zero, rhs);
                let cover = new_value!(func_data).store(r_binary, result);
                context.add_inst(r_binary);
                context.add_inst(cover);

                // end block
                let end_bb = context.new_block(None, true);
                let func_data = context.func_data();
                let branch = new_value!(func_data).branch(l_binary, end_bb, false_bb);
                add_inst!(func_data, base_bb, branch);

                let result = new_value!(func_data).load(result);
                context.add_inst(result);
                Ok(result)
            }
        }
    }
}

impl GenerateIr<Value> for LAndExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.generate_on(context),
            LAndExp::LAndOpExp(op_exp) => {
                // short circuit

                // base block
                let func_data = context.func_data();
                let zero = new_value!(func_data).integer(0);
                let lhs = op_exp.l_and_exp.generate_on(context)?;
                let func_data = context.func_data();
                let l_binary = new_value!(func_data).binary(BinaryOp::NotEq, lhs, zero);
                context.add_inst(l_binary);
                let result = context.alloc_and_store(l_binary, Type::get_i32());
                let base_bb = context.block.unwrap();

                // true block
                let true_bb = context.new_block(Some("%and_else".into()), false);
                let rhs = op_exp.eq_exp.generate_on(context)?;
                let func_data = context.func_data();
                let r_binary = new_value!(func_data).binary(BinaryOp::NotEq, rhs, zero);
                let cover = new_value!(func_data).store(r_binary, result);
                context.add_inst(r_binary);
                context.add_inst(cover);

                // end block
                let end_bb = context.new_block(None, true);
                let func_data = context.func_data();
                let branch = new_value!(func_data).branch(l_binary, true_bb, end_bb);
                add_inst!(func_data, base_bb, branch);

                let result = new_value!(func_data).load(result);
                context.add_inst(result);
                Ok(result)
            }
        }
    }
}

impl GenerateIr<Value> for EqExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.generate_on(context),
            EqExp::EqOpExp(op_exp) => {
                let lhs = op_exp.eq_exp.generate_on(context)?;
                let rhs = op_exp.rel_exp.generate_on(context)?;
                let func_data = context.func_data();
                let value = new_value!(func_data).binary(op_exp.eq_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for RelExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.generate_on(context),
            RelExp::RelOpExp(op_exp) => {
                let lhs = op_exp.rel_exp.generate_on(context)?;
                let rhs = op_exp.add_exp.generate_on(context)?;
                let func_data = context.func_data();
                let value = new_value!(func_data).binary(op_exp.rel_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for AddExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            AddExp::MulExp(mul) => mul.generate_on(context),
            AddExp::AddOpExp(op_exp) => {
                let lhs = op_exp.add_exp.generate_on(context)?;
                let rhs = op_exp.mul_exp.generate_on(context)?;
                let func_data = context.func_data();
                let value = new_value!(func_data).binary(op_exp.add_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for MulExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            MulExp::UnaryExp(unary) => unary.generate_on(context),
            MulExp::MulOpExp(mul_op_exp) => {
                let lhs = mul_op_exp.mul_exp.generate_on(context)?;
                let rhs = mul_op_exp.unary_exp.generate_on(context)?;
                let func_data = context.func_data();
                let value = new_value!(func_data).binary(mul_op_exp.mul_op.into(), lhs, rhs);
                context.add_inst(value);
                Ok(value)
            }
        }
    }
}

impl GenerateIr<Value> for UnaryExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.generate_on(context),

            UnaryExp::UnaryOpExp(unary_op_exp) => {
                let exp = unary_op_exp.unary_exp.generate_on(context)?;
                let func_data = context.func_data();
                let zero = new_value!(func_data).integer(0);
                let value = match unary_op_exp.unary_op {
                    UnaryOp::Pos => return Ok(exp),
                    UnaryOp::Neg => new_value!(func_data).binary(BinaryOp::Sub, zero, exp),
                    UnaryOp::Not => new_value!(func_data).binary(BinaryOp::Eq, exp, zero),
                };
                context.add_inst(value);
                Ok(value)
            }

            UnaryExp::FuncCall(func_call) => {
                let mut callee = None;
                let mut args = vec![];
                for arg in &func_call.args {
                    args.push(arg.generate_on(context)?);
                }
                for (&func, data) in context.program.funcs() {
                    if data.name() == global_ident!(func_call).as_str() {
                        callee = Some(func);
                        break;
                    }
                }
                if callee.is_none() {
                    return Err(AstError::FunctionNotFoundError(func_call.ident.clone()));
                }

                let func_data = context.func_data();
                let call = new_value!(func_data).call(callee.unwrap(), args);
                context.add_inst(call);
                Ok(call)
            }
        }
    }
}

impl GenerateIr<Value> for PrimaryExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            PrimaryExp::Number(number) => number.generate_on(context),
            PrimaryExp::LValExp(l_val) => l_val.generate_on(context),
            PrimaryExp::Exp(exp) => exp.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for LValExp {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match context.syb_table.lookup_item(&self.ident) {
            Some(item) => match &item {
                SymbolItem::Const(symbol) => symbol.value.clone().generate_on(context),
                SymbolItem::ConstArray(_) => todo!(),
                _ => {
                    let lval = LVal {
                        ident: self.ident.clone(),
                        array_index: self.array_index.clone(),
                    };
                    let alloc = lval.generate_on(context)?;
                    let load = new_value!(context.func_data()).load(alloc);
                    context.add_inst(load);
                    Ok(load)
                }
            },
            None => Err(AstError::SymbolNotFoundError(self.ident.clone())),
        }
    }
}

impl GenerateIr<Value> for Number {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        match self {
            Number::Int(int) => int.generate_on(context),
        }
    }
}

impl GenerateIr<Value> for i32 {
    fn generate_on(&self, context: &mut Context) -> Result<Value, AstError> {
        let int = new_value!(context.func_data()).integer(*self);
        Ok(int)
    }
}
